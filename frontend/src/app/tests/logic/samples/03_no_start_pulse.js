module.exports = {
  "nodes": {
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
        "x": 1097,
        "y": 622
      }
    },
    "4652b79c-603b-4add-9164-92508be43fdf": {
      "data": {
        "type": "simple_flow_block",
        "value": {
          "options": {
            "type": "operation",
            "outputs": [
              {
                "type": "pulse"
              },
              {
                "type": "string"
              }
            ],
            "message": "Get today's max temperature for %i1",
            "inputs": [
              {
                "type": "pulse"
              },
              {
                "type": "enum",
                "enum_namespace": "536bf266-fabf-44a6-ba89-a0c71b8db608",
                "enum_name": "get_locations"
              }
            ],
            "block_function": "services.536bf266-fabf-44a6-ba89-a0c71b8db608.get_today_max_in_place"
          },
          "synthetic_input_count": 1,
          "synthetic_output_count": 1
        }
      },
      "position": {
        "x": 902.5,
        "y": 531
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
    "099ee247-851d-41bb-819c-5347247cd06a": {
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
        "x": 1094,
        "y": 474
      }
    }
  },
  "edges": [
    {
      "from": {
        "id": "099ee247-851d-41bb-819c-5347247cd06a",
        "output_index": 0
      },
      "to": {
        "id": "4652b79c-603b-4add-9164-92508be43fdf",
        "input_index": 1
      }
    },
    {
      "from": {
        "id": "4652b79c-603b-4add-9164-92508be43fdf",
        "output_index": 1
      },
      "to": {
        "id": "1545b4f2-8b4f-4c59-ae0b-a0a0e5d6746c",
        "input_index": 2
      }
    },
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
        "id": "4652b79c-603b-4add-9164-92508be43fdf",
        "output_index": 0
      },
      "to": {
        "id": "1545b4f2-8b4f-4c59-ae0b-a0a0e5d6746c",
        "input_index": 0
      }
    }
  ]
}
