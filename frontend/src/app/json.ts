enum JSONType {
    Null,
    Boolean,
    String,
    List,
    Number,
    Map,
}

const GetTypeOfJson = (function(element): JSONType {
    if (element === null) {
        return JSONType.Null;
    } else if (typeof element === 'boolean') {
        return JSONType.Boolean;
    } else if (typeof element === 'string') {
        return JSONType.String;
    } else if (element.length !== undefined) {
        return JSONType.List;
    } else if (element.toFixed !== undefined) {
        return JSONType.Number;
    } else {
        return JSONType.Map;
    }
})

export { GetTypeOfJson, JSONType };
