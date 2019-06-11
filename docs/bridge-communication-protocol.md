# Bridge communication protocol

**Warning**: This protocol is in development and most of it **will change** later!

This document defines the protocol that a bridge has to implement to expose its functionalities on the Plaza platform. There's four main contexts for this protocol:

1. Bridge initialization
2. Event notification
3. Function calls
4. Data callback execution

All communication is performed through the bridge websocket connection to the bridge `communication` endpoint, encoded as JSON.

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
   * `operation`, defined on scratch as [Stack Block](https://en.scratch-wiki.info/wiki/Stack_Block), can be concatenated.
   * `getter`, defined on scratch as [Reporter Block](https://en.scratch-wiki.info/wiki/Reporter_Block), can be used in place of a value.
* `block_result_type` is reserved and not used yet, set it to `null`.
* `message`: The message shown on the block. All block arguments **must** be present on the message represented as `%<index-of-argument>` (index starts on 1). 
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
