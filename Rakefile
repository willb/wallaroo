require 'rubygems'
require 'rake'
require 'erb'

require './version'

PKG_RELEASE=ENV['PKG_RELEASE'] || 1

def pkg_version
  ::Wallaroo::Version.as_string
end

def pkg_version_component(which)
  ::Wallaroo::Version.const_get(which.to_s.upcase)
end

def make_patches
  numbered_files = ENV['SIMPLE_GIT_PATCH_NAMES'] ? "--numbered-files" : ""
  sh "git format-patch #{numbered_files} -o SOURCES v#{pkg_version}"
end

def list_patches
  Dir["SOURCES/*.patch"].sort.map {|f| f.gsub("SOURCES/", "")}
end

def printable_patch_list(ls=nil)
  ls||=list_patches
  result, = ls.inject([[], 0]) do |acc, value|
    ls_so_far, idx = acc
    [ls_so_far << "Patch#{idx}: #{value}", idx + 1]
  end
  result.join("\n")
end

def pkg_name
  return 'wallaby2'
end

def pkg_spec
  return pkg_name() + ".spec"
end

def pkg_rel
  PKG_RELEASE
end

def pkg_source
  return "#{pkg_name}-#{pkg_version}.tar.gz"
end

def pkg_dir
  return pkg_name() + "-" + pkg_version()
end

def rpm_dirs
  return %w{BUILD BUILDROOT RPMS SOURCES SPECS SRPMS}
end

def commit_version
  old_version = pkg_version
  [:MAJOR, :MINOR, :PATCH, :BUILD].each {|vc| ::Mrg::Grid::Config::Version.send(:remove_const, vc)}
  load 'lib/mrg/grid/config/version.rb'
  new_version = pkg_version
  message = "bumping version from #{old_version} to #{new_version}"
  sh "git commit -m '#{message}' lib/mrg/grid/config/version.rb"
  sh "git tag v#{new_version}"
  sh "git push origin master v#{new_version}" 
end

def bump_version_component(vc)
  old_v=pkg_version_component(vc)
  set_version_component(vc, old_v+1)
end

def set_version_component(vc, new_v)
  old_v=pkg_version_component(vc)
  vc = vc.to_s.upcase
  sh "sed -i 's/#{vc}=#{old_v}/#{vc}=#{new_v}/' ./version.rb"
end

def clear_build
  set_version_component(:build, '"nil"') if pkg_version_component(:build)
end

desc "bump the patchlevel"
task :bump_patch do
  bump_version_component(:patch)
  clear_build
  commit_version
end

desc "bump the minor version number"
task :bump_minor do
  bump_version_component(:minor)
  set_version_component(:patch, 0)
  clear_build
  commit_version
end

desc "bump the major version number"
task :bump_major do
  bump_version_component(:major)
  [:patch, :minor].each {|vc| set_version_component(vc, 0)}
  clear_build
  commit_version
end

def package_prefix
  "#{pkg_name}-#{pkg_version}"
end

def pristine_name
  "#{package_prefix}.tar.gz"
end

desc "upload a pristine tarball for the current release to fedorahosted"
task :upload_pristine => [:pristine] do
  raise "Please set FH_USERNAME" unless ENV['FH_USERNAME']
  sh "scp #{pristine_name} #{ENV['FH_USERNAME']}@fedorahosted.org:grid"
end

desc "generate a pristine tarball for the tag corresponding to the current version"
task :pristine => [:gen_env_file] do
  sh "git archive --format=tar v#{pkg_version} --prefix=#{package_prefix}/ | gzip -9nv > #{pristine_name}"
end

desc "create RPMs"
task :rpms => [:build, :tarball, :patches, :gen_spec] do
  FileUtils.cp [pkg_spec()], 'SPECS'
  sh "rpmbuild --define=\"_topdir \${PWD}\" -ba SPECS/#{pkg_spec}"
end

task :patches do
  make_patches
end

desc "Generate the specfile"
task :gen_spec => [:make_rpmdirs] do
  make_patches
  File.open(pkg_spec, "w") do |f|
    f.write(ERB.new(File.read("#{pkg_spec}.in")).result(binding))
  end
end

# desc "Create an environment file from the sysconfig file"
# task :gen_env_file do
#   sh "sed 's/^export //g' < etc/sysconfig/wallaby-agent > etc/sysconfig/wallaby-agent-env"
# end

desc "Create a tarball"
task :tarball => [:make_rpmdirs, :gen_spec, :gen_env_file, :pristine] do
  FileUtils.cp pristine_name, 'SOURCES'
end

desc "Make dirs for building RPM"
task :make_rpmdirs => :clean do
  FileUtils.mkdir pkg_dir()
  FileUtils.mkdir rpm_dirs()
end

desc "Clean up after an RPM build"
task :clean do
  require 'fileutils'
  FileUtils.rm_r [pkg_dir(), 'pkg', rpm_dirs(), pkg_spec()], :force => true
end
