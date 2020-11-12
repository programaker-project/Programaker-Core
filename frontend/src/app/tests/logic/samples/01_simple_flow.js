module.exports = {
  "nodes": {
    "fc4bd63f-d4ff-4e15-8b09-a224e6e1c635": {
      "data": {
        "type": "enum_value_block",
        "value": {
          "options": {
            "enum_namespace": "536bf266-fabf-44a6-ba89-a0c71b8db608",
            "enum_name": "get_locations"
          },
          "value_id": "12/36/057/7",
          "value_text": "Vigo"
        }
      },
      "position": {
        "x": 1046,
        "y": 470
      }
    },
    "f1b8670c-0001-4417-8a39-2c52f5140383": {
      "data": {
        "type": "enum_value_block",
        "value": {
          "options": {
            "enum_namespace": "de5baefb-13da-457e-90a5-57a753da8891",
            "enum_name": "get_known_channels"
          },
          "value_id": "-137414823",
          "value_text": "Bot testing"
        }
      },
      "position": {
        "x": 1069,
        "y": 624
      }
    },
    "918294c3-1e7d-4b2f-ab73-77188a4b89b0": {
      "data": {
        "type": "simple_flow_block",
        "value": {
          "options": {
            "type": "getter",
            "outputs": [
              {
                "type": "string"
              }
            ],
            "message": "Get today's max temperature for %i1",
            "inputs": [
              {
                "type": "enum",
                "enum_namespace": "536bf266-fabf-44a6-ba89-a0c71b8db608",
                "enum_name": "get_locations"
              }
            ],
            "block_function": "services.536bf266-fabf-44a6-ba89-a0c71b8db608.get_today_max_in_place"
          },
          "synthetic_input_count": 0,
          "synthetic_output_count": 0
        }
      },
      "position": {
        "x": 1008.5,
        "y": 535
      }
    },
    "2b3b54d4-bcfa-4137-825f-ca52e2be4e96": {
      "data": {
        "type": "direct_value_block",
        "value": {
          "value": 0,
          "type": "integer"
        }
      },
      "position": {
        "x": 1277,
        "y": 200
      }
    },
    "1545b4f2-8b4f-4c59-ae0b-a0a0e5d6746c": {
      "data": {
        "type": "simple_flow_block",
        "value": {
          "options": {
            "type": "operation",
            "outputs": [
              {
                "type": "pulse"
              }
            ],
            "message": "On channel %i1 say %i2",
            "inputs": [
              {
                "type": "pulse"
              },
              {
                "type": "enum",
                "enum_namespace": "de5baefb-13da-457e-90a5-57a753da8891",
                "enum_name": "get_known_channels"
              },
              {
                "type": "string"
              }
            ],
            "icon": "http://192.168.1.35:8888/api/v0/assets/icons/de5baefb-13da-457e-90a5-57a753da8891",
            "block_function": "services.de5baefb-13da-457e-90a5-57a753da8891.send_message"
          },
          "synthetic_input_count": 1,
          "synthetic_output_count": 1
        }
      },
      "position": {
        "x": 866.5,
        "y": 679
      }
    },
    "0b2f1836-aaf3-4e13-84b9-8041c3b5b4b8": {
      "data": {
        "type": "direct_value_block",
        "value": {
          "value": 11,
          "type": "integer"
        }
      },
      "position": {
        "x": 883,
        "y": 237
      }
    },
    "032d2a4e-bfe2-4635-a1cf-dc62692eead7": {
      "data": {
        "type": "simple_flow_block",
        "value": {
          "options": {
            "type": "trigger",
            "outputs": [
              {
                "type": "pulse"
              }
            ],
            "message": "When all true",
            "inputs": [
              {
                "type": "boolean"
              },
              {
                "type": "boolean"
              },
              {
                "type": "boolean"
              }
            ],
            "icon": "/assets/logo-dark.png",
            "extra_inputs": {
              "type": "boolean",
              "quantity": "any"
            },
            "block_function": "trigger_when_all_true"
          },
          "synthetic_input_count": 0,
          "synthetic_output_count": 1
        }
      },
      "position": {
        "x": 867.5,
        "y": 391.5
      }
    },
    "0f82640f-b40b-4053-981d-1fe0b2c17de0": {
      "data": {
        "type": "simple_flow_block",
        "value": {
          "options": {
            "icon": "/assets/logo-dark.png",
            "message": "Are all equals?",
            "block_function": "operator_equals",
            "type": "getter",
            "inputs": [
              {
                "type": "any"
              },
              {
                "type": "any"
              },
              {
                "type": "any"
              }
            ],
            "extra_inputs": {
              "type": "any",
              "quantity": "any"
            },
            "outputs": [
              {
                "type": "boolean"
              }
            ]
          },
          "synthetic_input_count": 0,
          "synthetic_output_count": 0
        }
      },
      "position": {
        "x": 722.5,
        "y": 293.5
      }
    },
    "0d4937a0-cdd3-4bf6-b8e8-8da2974b1330": {
      "data": {
        "type": "simple_flow_block",
        "value": {
          "options": {
            "icon": "/assets/logo-dark.png",
            "message": "Are all equals?",
            "block_function": "operator_equals",
            "type": "getter",
            "inputs": [
              {
                "type": "any"
              },
              {
                "type": "any"
              },
              {
                "type": "any"
              },
              {
                "type": "any"
              }
            ],
            "extra_inputs": {
              "type": "any",
              "quantity": "any"
            },
            "outputs": [
              {
                "type": "boolean"
              }
            ]
          },
          "synthetic_input_count": 0,
          "synthetic_output_count": 0
        }
      },
      "position": {
        "x": 1099.5,
        "y": 283.5
      }
    },
    "ad97e5d1-c725-4cc6-826f-30057f239635": {
      "data": {
        "type": "simple_flow_block",
        "value": {
          "options": {
            "icon": "/assets/logo-dark.png",
            "message": "UTC time",
            "block_function": "flow_utc_time",
            "type": "getter",
            "outputs": [
              {
                "name": "hour",
                "type": "integer"
              },
              {
                "name": "minute",
                "type": "integer"
              },
              {
                "name": "second",
                "type": "integer"
              }
            ],
            "inputs": []
          },
          "synthetic_input_count": 0,
          "synthetic_output_count": 0
        }
      },
      "position": {
        "x": 881.5,
        "y": 117.5
      }
    }
  },
  "edges": [
    {
      "from": {
        "id": "f1b8670c-0001-4417-8a39-2c52f5140383",
        "output_index": 0
      },
      "to": {
        "id": "1545b4f2-8b4f-4c59-ae0b-a0a0e5d6746c",
        "input_index": 1
      }
    },
    {
      "from": {
        "id": "918294c3-1e7d-4b2f-ab73-77188a4b89b0",
        "output_index": 0
      },
      "to": {
        "id": "1545b4f2-8b4f-4c59-ae0b-a0a0e5d6746c",
        "input_index": 2
      }
    },
    {
      "from": {
        "id": "fc4bd63f-d4ff-4e15-8b09-a224e6e1c635",
        "output_index": 0
      },
      "to": {
        "id": "918294c3-1e7d-4b2f-ab73-77188a4b89b0",
        "input_index": 0
      }
    },
    {
      "from": {
        "id": "032d2a4e-bfe2-4635-a1cf-dc62692eead7",
        "output_index": 0
      },
      "to": {
        "id": "1545b4f2-8b4f-4c59-ae0b-a0a0e5d6746c",
        "input_index": 0
      }
    },
    {
      "from": {
        "id": "ad97e5d1-c725-4cc6-826f-30057f239635",
        "output_index": 2
      },
      "to": {
        "id": "0d4937a0-cdd3-4bf6-b8e8-8da2974b1330",
        "input_index": 1
      }
    },
    {
      "from": {
        "id": "ad97e5d1-c725-4cc6-826f-30057f239635",
        "output_index": 1
      },
      "to": {
        "id": "0d4937a0-cdd3-4bf6-b8e8-8da2974b1330",
        "input_index": 0
      }
    },
    {
      "from": {
        "id": "2b3b54d4-bcfa-4137-825f-ca52e2be4e96",
        "output_index": 0
      },
      "to": {
        "id": "0d4937a0-cdd3-4bf6-b8e8-8da2974b1330",
        "input_index": 2
      }
    },
    {
      "from": {
        "id": "ad97e5d1-c725-4cc6-826f-30057f239635",
        "output_index": 0
      },
      "to": {
        "id": "0f82640f-b40b-4053-981d-1fe0b2c17de0",
        "input_index": 0
      }
    },
    {
      "from": {
        "id": "0b2f1836-aaf3-4e13-84b9-8041c3b5b4b8",
        "output_index": 0
      },
      "to": {
        "id": "0f82640f-b40b-4053-981d-1fe0b2c17de0",
        "input_index": 1
      }
    },
    {
      "from": {
        "id": "0f82640f-b40b-4053-981d-1fe0b2c17de0",
        "output_index": 0
      },
      "to": {
        "id": "032d2a4e-bfe2-4635-a1cf-dc62692eead7",
        "input_index": 0
      }
    },
    {
      "from": {
        "id": "0d4937a0-cdd3-4bf6-b8e8-8da2974b1330",
        "output_index": 0
      },
      "to": {
        "id": "032d2a4e-bfe2-4635-a1cf-dc62692eead7",
        "input_index": 1
      }
    }
  ]
}
