# Bridge communication protocol

**Warning**: This protocol is in development and most of it **will change** later!

This document defines the protocol that a bridge has to implement to expose its functionalities on the Plaza platform. There's four main contexts for this protocol:

1. Bridge initialization
2. Event notification
3. Function calls
4. Service registration
5. Data callback execution

All communication is performed through the bridge websocket connection to the bridge `communication` endpoint, encoded as JSON. **Note that messages must be sent in a single websocket frame, as fragmentation is not supported.**

## Bridge initialization

As a bridge websocket connects to the platform it must send the following information (JSON encoded):

```json
{
    "type": "CONFIGURATION",
    "value": { 
        "blocks": [ <block definition> ],
        "is_public": false,
        "service_name": "<name of the service>"
    }
}
```

For example, a minimal configuration, with no blocks, might be the following (send it in a single line):

```json
{ "type": "CONFIGURATION", "value": { "blocks": [], "is_public": false, "service_name": "comm-test" } }
```

Expanded:
```json
{
    "type": "CONFIGURATION",
    "value": { 
        "blocks": [],
        "is_public": false,
        "service_name": "comm-test"
    }
}
```

Opening a websocket and sending this information will show the bridge as connected, but no blocks will be defined.

Blocks definitions can be of 2 main groups:
* Operations and Getters. Are called when a program runs then, expecting a return value.
* Triggers. Are run proactively by the bridge, and triggered by sending event notifications

### Operations and getters

Operation and getter blocks are defined through the following objects

```json
{
    "id": "<internal id for the operation>",
    "function_name": "<internal id for the operation>",
    "block_type": "<operation | getter>",
    "block_result_type": null,
    "message": "<block message>", 
    "arguments": [ <argument definition> ]
 }
```


* `id` and `function_name`: Define the unique `id` given by the bridge to the operation. **Note** that the internal id operation is duplicated on the `id` and function name. This is a known problem and is something to be fixed. It's recommended to use the the same value on `id` and `function_name` for clarity.
* `block_type`: Define the nature of the block, can be of two types:
   * `operation`, defined on Scratch as [Stack Block](https://en.scratch-wiki.info/wiki/Stack_Block), can be concatenated.
   * `getter`, defined on Scratch as [Reporter Block](https://en.scratch-wiki.info/wiki/Reporter_Block), can be used in place of a value.
* `block_result_type` is reserved and not used yet, set it to `null`.
* `message`: The message shown on the block. All block arguments **must** be present on the message represented as `%<index-of-argument>` (index starts on 0). 
* `arguments`: the arguments that the operation considers to perform an operation.

#### Arguments

A bridge might require knowing the values of the context to perform a certain operation, these can be passed through arguments, there are 3 types of arguments:
* Values: Normal values which have been resolved.
* Variable names: The name of an **unresolved** variable.
* Selections: A single option of a dynamic list provided by the bridge during "design" time.

##### Values

Simple value arguments can be defined with the following JSON object 

```json
{
    "type": "<string | integer | float | boolean>",
    "default": "<string-encoded default value>"
}
```

The type of the argument defines the UI appearance of the software keyboard shown when selecting the block.

##### Variable names

Variable name arguments can be defined with the following JSON object

```json
{
    "type": "variable",
    "class": "<single|list>
}
```

The class of the argument defines which variables can the user choses among, normal variables or list variables.
The main application of these variable names are **Trigger blocks**. 

##### Selections

See `Data callback execution` for further understanding of these operations.

#### Example

A bridge simple block operation can be configured using this JSON object:

```json
{
    "type": "CONFIGURATION",
    "value": { 
        "blocks": [
            {
                "id": "max-num",
                "function_name": "max-num",
                "block_type": "getter",
                "block_result_type": null,
                "message": "Max of %1 and %2",
                "arguments": [
                    {
                        "type": "integer",
                        "default": "0"
                    },
                    {
                        "type": "integer",
                        "default": "1"
                    }
                ]
            }
        ],
        "is_public": false,
        "service_name": "comm-test"
    }
}
```

In a single line:

```json
{ "type": "CONFIGURATION", "value": { "blocks": [ { "id": "max-num", "function_name": "max-num", "block_type": "getter", "block_result_type": null, "message": "Max of %1 and %2", "arguments": [ { "type": "integer", "default": "0" }, { "type": "integer", "default": "1" } ] } ], "is_public": false, "service_name": "comm-test" } }
```

See **Function calls** for how to implement a bridge that answers these types of blocks.

### Trigger blocks

Trigger blocks represent an operation that is started when a certain event happens, they correspond to Scratch [Hat Blocks](https://en.scratch-wiki.info/wiki/Hat_Block) and are defined through the following JSON schema:

```json
{
    "id": "<internal id for the operation>",
    "function_name": "<internal id for the operation>",
    "block_type": "trigger",
    "key": "<key>",
    "message": "<block message>", 
    "arguments": [ <argument definition> ],
    "save_to": <null | argument reference definition>,
    "expected_value": <null | argument reference definition>
 }
```

* `id` and `function_name`: Define the unique `id` given by the bridge to the operation. **Note** that the internal id operation is duplicated on the `id` and function name. This is a known problem and is something to be fixed. It's recommended to use the the same value on `id` and `function_name` for clarity.
* `block_type`: Define the nature of the block. Trigger blocks must be defined as `trigger`.
* `key`: The event channel (of the ones from the bridge) where the event will be expected.
* `message`: The message shown on the block. All block arguments **must** be present on the message represented as `%<index-of-argument>` (index starts on 0). 
* `arguments`: the arguments that the operation considers to perform an operation.
* `save_to` references an argument, this argument (must be a variable name), will store the content of the event.
* `expected_value` references an argument, the value of this argument will be checked against the event `content`.

#### Argument references

Argument references are specified with the following schema:

```json
{ 
    "type": "argument",
    "index": <index of the referenced argument>
}
```

For example, a trigger block might be defined with the following structure:

```json
{
    "type": "CONFIGURATION",
    "value": { 
        "blocks": [
            {
                "id": "temperature-reading",
                "function_name": "temperature-reading",
                "key": "temperature-reading",
                "block_type": "trigger",
                "message": "On temperature reading. Set %1",
                "arguments": [
                    {
                        "type": "variable",
                        "class": "single"
                    }
                ],
                "save_to": {
                    "type": "argument",
                    "index": 0
                },
                "expected_value": null
            }
        ],
        "is_public": false,
        "service_name": "comm-test"
    }
}
```

In a single line: 
```json
{"type": "CONFIGURATION", "value": {"blocks": [{"id": "temperature-reading", "function_name": "temperature-reading", "key": "temperature-reading", "block_type": "trigger", "message": "On temperature reading. Set %1", "arguments": [{"type": "variable", "class": "single"}], "save_to": {"type": "argument", "index": 0}, "expected_value": null}], "is_public": false, "service_name": "comm-test"}}
```

## Event notification

After the configuration above is loaded, events can be used to trigger the `temperature-reading` block. These events can be sent through the websocket by the bridge at any time and take the following shape:

```json
{
    "type": "NOTIFICATION",
    "key": "<key>",
    "to_user": <null|user-id>,
    "content": "<event content>",
    "value": "<full event information>",
}
```
* `key` field has to match with the one on the *Trigger block*, for it to be triggered.
* `to_user` might be a bridge-specific user-Id to send the event to a specific user or `null` for it to be sent to all (registered and authorized) users.
* The value of the `content` field is the one used for the *Trigger block* `save_to` and `expected_value` operative.
* The `value` entry might contain all the relevant information of the event. Not that this is saved on the platform, but at this point cannot be accessed from outside.

Thus, a notification can be sent for the configuration sent with a JSON object like this:

```json
{
    "type": "NOTIFICATION",
    "key": "temperature-reading",
    "to_user": null,
    "content": 9999,
    "value": { "sensor": "hot stuff", "reading": 9999 }
}
```

In a single line:

```json
{ "type": "NOTIFICATION", "key": "temperature-reading", "to_user": null, "content": "9999", "value": { "sensor": "hot stuff", "reading": 9999 } }
```

## Function calls

On the **Operations and getters** section we defined a block, which can be run by plaza
```json
{
    "id": "max-num",
    "function_name": "max-num",
    "block_type": "getter",
    "block_result_type": null,
    "message": "Max of %1 and %2",
    "arguments": [
        {
            "type": "integer",
            "default": "0"
        },
        {
            "type": "integer",
            "default": "1"
        }
    ]
}
```

when this happens, a message is sent **to the bridge** by the platform with the following schema:

```json
{
    "type": "FUNCTION_CALL",
    "message_id": "<message identifier>",
    "value": {
        "function_name": "<name of the function to perform>",
        "argument": [ <values of the arguments of the function> ]
    },
    "user_id": "<id of the user that owns the running program>"
}
```

For example:

```json
{
    "type":"FUNCTION_CALL",
    "message_id":"30d504f1-bdc6-4602-8f02-f72b29572dde",
    "value": {
        "function_name":"max-num",
        "arguments":[ "5", "7" ]
    },
    "user_id":"08240ff1-8414-4daa-a089-21100cbf8ca1"
}
```

After the bridges computes the maximum of `5` and `7`, it must answer with the following format:

```json
{
    "message_id": "<same message id as the function call>",
    "success": true,
    "result": <result value>
}
```

In our example (and already in a single line)

```json
{ "message_id":"30d504f1-bdc6-4602-8f02-f72b29572dde", "success": true, "result": 7}
```

Note that the `result` field doesn't have to be stringified.

Alternatively, if the operation fails, it can be answered signaling no success, but no error tracing is yet implemented:
```json
{
    "message_id": "<same message id as the function call>",
    "success": false
}
```

## Service registration

TODO

## Data callback execution

**Note**: this section is yet to be written, as future development will probably include major changes on it's operation and it's not required for most usages. 