# Refactored Wallaroo tree design

## Motivation

* We want to represent hierarchically-structured write-once data objects in trees.
* We want to store these trees and exploit redundancy to allow us to keep old versions around indefinitely.
* We want to avoid performance pitfalls, including:
  * Storing too many object references in a single tree object (and thus making the fixed storage cost for even small changes very high)
  * Storing too few object references in a single tree object (and thus making the fixed database-write count cost for even small changes very high)
* We want a more robust interface with better type-safety -- it shouldn't be up to tree users to figure out whether a hash represents a subtree or an object, for example

The current `wallaroo_tree` design uses an Erlang `gb_tree` to represent the name-value pairs at each level of the hierarchy.  Every name maps to a hash, which is the key of an object in the database.  Some hashes are keys for sub-hierarchies (serialized `gb_tree` structures) and some are keys for arbitrary Erlang terms.  A *path* is a sequence of names N<sub>0</sub>, ..., N<sub>k</sub> such that N<sub>0</sub> is the name that the root tree maps to a subtree object T<sub>1</sub>; generally, N<sub>k</sub> is the name that T<sub>k</sub> maps to an object (either a subtree or a general serialized term).  

Since every change is non-destructive we are interested in saving every version of the hierarchy after updates (it is a persistent, functional tree), when we make a change to the object stored at some path N<sub>0</sub>, ..., N<sub>k</sub>, we must change every tree between T<sub>k</sub> and the root.  (See [this blog post](http://chapeau.freevariable.com/2011/06/restful-manipulation-of-versioned-data.html) for details on a couple of general approaches.)  Furthermore, since we would like to store arbitrary quantities of historical trees in a space-efficient manner, we need to share unchanged sub-hierarchies among versions (or even within versions).  So the question is this:  how do we minimize the number of database reads and writes for paths of a given length while also minimizing the size of serialized objects that must be changed to reflect a modification to a single object.  We could, of course, bucket large hierarchies by hashing names into several subtrees (in a way that would be transparent to users of the path-based API).

## Particulars

The lower bound of serialized `gb_tree` overhead appears to be about 16 bytes per element; the upper bound is around 19 bytes per element.  Assuming a tree that maps ~27 byte names (corresponding to typical datacenter hostnames) to SHA-1 hashes (20 bytes), a serialized tree of

* 4 elements will take 247 bytes;
* 16 elements will take 962 bytes;
* 64 elements will take 3842 bytes;
* 256 elements will take 15522 bytes;
* 1024 elements will take 62395 bytes;
* 4096 elements will take 252859 bytes;
* 16384 elements will take 1021100 bytes; and
* 65536 elements will take 4117676 bytes

For the Wallaby use case, in which we might realistically store on the order of 2<sup>14</sup> node names and hashes at the path `/nodes` in current deployments (and would like plenty of headroom to scale up), this is an unacceptable overhead:  if we are to add a group to a node's membership list (probably involving modifying ~20 bytes of actual data), for example, we must store the new node term, a megabyte of rewritten tree structure for the `/nodes` subtree, and about a kilobyte of rewritten tree structure for the root tree.  Considering that the Wallaby use case is substantially less demanding than other use cases we can imagine for Wallaroo trees (e.g. filesystem metadata), this is a real problem.

### Bucketed subtrees

How many objects should we store in the database for each level of the hierarchy?  Put another way, can we restrict fanout somehow?  It perhaps makes sense to have two kinds of subtrees:

1.  *accessible subtrees* whose leaves contain API-accessible objects (i.e. those that are addressable via a path), and
2.  *internal subtrees*, whose leaves contain internal-use-only subtrees representing *buckets*; a *bucket* is a subtree containing other buckets or API-accessible objects, but it cannot be accessed via a path (paths are transparently converted to bucket operations if necessary).

If we convert accessible subtrees with more than 64 buckets to internal subtrees with 64 buckets, we'll need three layers of buckets to cover 2<sup>14</sup> objects (indeed, 2<sup>18</sup> objects).  Let's consider, as an example, the idea of updating the term pointed to by `/nodes/node-1234.local.domain.com`, with at most 2<sup>14</sup> nodes as siblings in the `/nodes` hierarchy.

1.  In the accessible-subtree case, this would involve 
  1. a database read (to get the root associated with the current state), 
  2. a database read (to get `nodes`), 
  3. a database read (to get the term corresponding to the stored hash pointed to by `node-1234.local.domain.com`), followed by 
  4. a write (the term), 
  5. a write (`nodes`), and 
  6. a write (the root).  
2.  In the bucketed-subtree case, this would involve
  1. a database read (to get the root associated with the current state),
  2. a database read (to get `nodes`), followed by identifying that `nodes` is a bucketed tree.  The first three 6-bit quantities in the hash of `node-1234.local.domain.com` are 31, 7, and 61; these will be our path into the buckets contained under `nodes`.  We will then need to perform
  3. a database read (to get `31`),
  4. a database read (to get `7`),
  5. a database read (to get `61`),
  6. a database read (to get the term corresponding to the stored hash pointed to by `node-1234.local.domain.com`), followed by
  7. a write (the term),
  8. a write (`61`),
  9. a write (`7`),
  10. a write (`31`),
  11. a write (`nodes`), and
  12. a write (the root).

In the accessible-subtree case, the writes involved would store the size of the serialized node object (presumably, this would be pretty small), approximately 1 MB to store the updated `/nodes` hierarchy, and approximately 1 KB to store the updated root, _for a total on the order of 1 MB of writes (and 1 MB of reads)_.  In the bucketed-subtree case, the writes involved would store the size of the serialized node object, approximately 4 KB for each of `nodes`, `31`, `7`, and `61`, and approximately 1 KB for the updated root, _for a total on the order of 16 KB of writes (and 16KB of reads)_.

The bucketed-tree case is a clear winner for space efficiency of updates (and also, probably, for speed of updates) but would involve substantially more database read activity in order to fetch all of the (transitive) children of a bucketed tree.  We'd need to evaluate its performance under some workloads to characterize whether or not this is an acceptable tradeoff.

### Storage considerations

Currently, objects are stored in the database in a relatively raw format:  if something looks like a hash, it is a hash; if something looks like a tree, it is a tree; in general, the current implementation relies on clients to know what sort of thing is stored at a given path.  We should change this so that objects have a more well-defined format:

    -type accessible_tree() :: {wallaroo_accessible_tree, tree()}.
    -type bucketed_tree() :: {wallaroo_bucketed_tree, tree()}.
    -type serialized_object() :: {wallaroo_object, any()}.

## Appendix:  Overhead-measurement code

    -module(tree_eval).
    -export([tree_size/1, overheads/1, powerof2_overheads/2]).
    
    overheads([_|_]=Sizes) ->
        [tree_size(X) || X <- Sizes].
    
    powerof2_overheads(Small, Large) ->
        overheads([trunc(math:pow(2,Y)) || Y <- lists:seq(Small,Large)]).
    
    tree_size(Count) -> 
        [N|_] = Nodes = ["node-" ++ integer_to_list(N) ++ ".local.domain.com" || N <- lists:seq(1,Count)],
        ExemplarHash = wallaroo_hash:as_bitstring(N),
        NodePairs = [{Node, ExemplarHash} || Node <- Nodes],
        Set = gb_trees:from_orddict(NodePairs),
        Sz = size(term_to_binary(Set)),
        {Count, length(lists:flatten(Nodes)) + 20 * length(Nodes), Sz}.

