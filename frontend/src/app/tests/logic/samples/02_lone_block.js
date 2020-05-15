module.exports = {
  "nodes": {
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
        "x": 973.5,
        "y": 484
      }
    }
  },
  "edges": []
}
