const static = require("node-static");
const dir = new static.Server("../");


require("http").createServer(function(request, response) {
    request.addListener("end", function() {
        dir.serve(request, response);
    }).resume();
}).listen(8080);
