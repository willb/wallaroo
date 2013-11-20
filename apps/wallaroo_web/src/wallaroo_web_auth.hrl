-define(STANDARD_AUTH(ReqData, Ctx),
	wallaroo_web_common:generic_auth(ReqData, 
					 Ctx, 
					 orddict:from_list([{'PUT', write}, 
							    {'DELETE', write}, 
							    {'GET', read}]), 
					 read)).

-define(USE_STANDARD_AUTH, is_authorized(ReqData, Ctx) -> 
	       wallaroo_web_common:generic_auth(ReqData, 
						Ctx, 
						orddict:from_list([{'PUT', write}, 
								   {'DELETE', write}, 
								   {'GET', read}]), 
						read)).

-define(USE_STANDARD_AUTH_WITH_DEFAULT(Default), is_authorized(ReqData, Ctx) -> 
	       wallaroo_web_common:generic_auth(ReqData, 
						Ctx, 
						orddict:from_list([{'PUT', write}, 
								   {'DELETE', write}, 
								   {'GET', read}]), 
						Default)).
