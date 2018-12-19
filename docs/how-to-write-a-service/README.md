# How to write a service

A service has principally two sides, a frontend side (as in, scratch blocks), and a backend side (the code run in the server).

## Frontend

The services are exposed to the users mostly in the form of scratch blocks. To make a new scratch block follow these steps:


### Defining the new block

To define a new block open the file `frontend/src/app/blocks/Toolbox.ts` and look for the `injectBlocks()` function.
There you can see that more functions are called, we'll create a new function: `injectTimeBlocks`.

On `injectTimeBlocks` we can define a new block to get the current UTC hour with the following:

```javascript
Blockly.Blocks['time_get_utc_hour'] = {
    init: function() {
        this.jsonInit({
            // This defines the ID of the block
            // note that its the same as in the first line
            'id': 'time_get_utc_hour',
            // Message appearing on the block
            'message0': 'Get current UTC hour',
            'args0': [
            ],
            'category': Blockly.Categories.event,
            // Finally we can define which colors it'll take
            // and its shape
            // Shape is `output_string` to have it behave
            // as a getter
            'extensions': ['colours_time', 'output_string']
        });
    }
};
```

For our new type of block we also want a new color, we can define it after the block with

```javascript
try {
    Blockly.Extensions.register('colours_time',  // Name of the new category
                        function() {
                            this.setColourFromRawValues_('#85CCB3',  // Block inner color
                                                         '#1D1D5F',  // Category circle border color
                                                         '#1D1D5F'   // Block border color
                                                        );
                        });
} catch (e) {
    // If the extension was registered before
    // this would have thrown an inocous exception
    if (!Toolbox.alreadyRegisteredException(e)) {
        throw e;
    }
 }
```


### Injecting the new block and category on the toolbox

On head for the function `injectToolbox()`, there, after the `const *category` constants, you can create a new constant like the following:

```javascript
const timeCategory = '<category name="Time" colour="#85CCB3" secondaryColour="#1D1D5F">' +
    // Note the block id on both the type and id.
    '<block type="time_get_utc_hour" id="time_get_utc_hour">' +
    '</block>' +
    '</category>';
```

Finally, add the new `timeCategory` to the `Blockly.Blocks.defaultToolbox` list found at the end of the function, on the position you consider adecuate.

With this we completed the frontend of the new service.


## Backend

Now that we can build programs with the new block, we need to implement this functionality on the server.

### Linking

At this point, if the server receives a block calling the new function it won't know which service it should trigger.
To fix this we can use the `linker`, for that open the file `backend/apps/automate_program_linker/src/automate_program_linker.erl`.

We'll add a new case above this block:

```erlang
%% No relinking
relink_value(Value) ->
    Value.
```

To to this, write this above of that block:

```erlang
%% Relink time_get_utc_hour
relink_value(Value = #{ ?TYPE := <<"time_get_utc_hour">>
                      }) ->
    #{ ?TYPE => ?COMMAND_CALL_SERVICE
     , ?ARGUMENTS => #{ ?SERVICE_ACTION => get_utc_hour
                      , ?SERVICE_ID => automate_services_time:get_uuid()
                      , ?SERVICE_CALL_VALUES => Value
                      }
     };
```

This will convert the `time_get_utc_hour` block into a service call block pointing to `automate_services_time`.

With this out of the way, we can concentrate on adding the new service to the server.

### Adding the service

To add a new service, create a new directory on `backend/app/automate_services_<service_name>`.
Inside add a `src/` directory, and inside it a file named `automate_services_<service_name>.erl`.

To add a service we'll just have to build a module with the following methods:

* `start_link/0`: Called when the service is started
* `get_description/0`: Returns a brief description of the service
* `get_uuid/0`: Returns an unique ID associated with the service a random UUID/GUID is OK
* `get_name/0`: Returns the name of the server
* `is_enabled_for_user/1`: Checks if the service is enabled for a given user
* `get_how_to_enable/1`: Returns instructions on how to enable service.
* `get_monitor_id/1`: Returns monitor associated with the service (if any).
* `call/4`: Handles a call to the service.

This way we can define a time service like this:


```erlang
%%%-------------------------------------------------------------------
%% @doc Timekeeping service main module.
%% @end
%%%-------------------------------------------------------------------

-module(automate_services_time).

%% Service API
-export([ start_link/0
        , get_description/0
        , get_uuid/0
        , get_name/0
        , is_enabled_for_user/1
        , get_how_to_enable/1
        , get_monitor_id/1
        , call/4
        ]).
%%====================================================================
%% Service API
%%====================================================================

%% No need to initialize service
start_link() ->
    ignore.

%% This will return a fine one: https://duckduckgo.com/?q=uuid&ia=answer
get_uuid() ->
    <<"0093325b-373f-4f1c-bace-4532cce79df4">>.

get_name() ->
    <<"Timekeeping">>.

get_description() ->
    <<"Timekeeping service.">>.

%% No monitor associated with this service
get_monitor_id(UserId) ->
    {error, not_found}.

call(get_utc_hour, _Values, Thread, _UserId) ->
    {{_Y1970, _Mon, _Day}, {Hour, _Min, _Sec}} = calendar:now_to_datetime(erlang:timestamp()),
    {ok, Thread, Hour}.

%% Is enabled for all users
is_enabled_for_user(_Username) ->
    {ok, true}.

%% No need to enable service
get_how_to_enable(_) ->
    {error, not_found}.
```

### Start the service on startup

To start (register) the module when the server boots, first we have to make it an application. For that write a file `automate_services_<name of service>.app.src` like the following:

```erlang
{application, automate_services_time,
 [
  {description, "Auto-mate timekeeping service."},
  {vsn, "0.0.0"},
  {registered, []},
  {mod, { automate_services_time_app, [] }},
  {applications, [ automate_service_registry
                 ]},
  {env, [
        ]},
  {modules, []},
  {licenses, ["Apache 2.0"]},
  {links, []}
 ]}.
```

This declares a new application, and on start it'll trigger the `start/2` call on `automate_services_time_app`.
To connect this we can write a file like the following:

```erlang
%%%-------------------------------------------------------------------
%% @doc Timekeeping service starter
%% @end
%%%-------------------------------------------------------------------

-module(automate_services_time_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, _} = automate_service_registry:register_public(automate_services_time),
    {ok, self()}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
```

It's mostly a skeleton, but as you can see on `automate_service_registry:register_public`, it takes care of registering the service.

Next step is to add this application as a dependency of another one, so that it start this. For that open `backend/apps/automate_services_all/src/automate_services_app.app.src`, and add `automate_services_time` to the `applications entry.`

After that, the only thing that remains is to add the module name to the `backend/rebar.conf` file, on the `relx → release` entry.

Now, we have added a new timekeeping service that we can extend and a block that makes use if it.

