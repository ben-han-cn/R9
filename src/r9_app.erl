-module(r9_app).
-export([run/1]).


run(ConfigFilePath)->
    r9_pipeline_builder:start_link(r9_config:load(ConfigFilePath)).
