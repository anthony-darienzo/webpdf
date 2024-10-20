const protocolPrefix = (window.location.protocol === 'https:') ? 'wss:' : 'ws:';

function createWebSocket(path) {
        return new WebSocket(protocolPrefix + '//' + location.host + path);
}

function registerSocketLoop(socket) {

    socket.addEventListener("open", (event) => {
	const init_msg = JSON.stringify({
            event: "initialize",
            message: "Hello webpdf-hs!"
	});
	socket.send(init_msg);
    });

    socket.addEventListener("message", (event) => {
	var msg = JSON.parse(event.data);
	if (msg.event === "reload") {
            console.log("Received reload notice from WebSocket.");
    	let new_url = msg.message + "?t=" + Date.now();
    	    PDFViewerApplication.open({
		url: new_url
    	    }); // PDFViewerApplication is in /web/viewer.mjs
	} else {
            console.log("Received socket event.");
	    console.log(msg);
	}
    });
    
    socket.addEventListener("close", (event) => {
	console.warn("WebSocket connection to host is closed.");
    });

    window.addEventListener("beforeunload", (event) => {
	socket.close();
    });
}

var socketsByUrl = {};

function fetchSocket(path) {
    let url = protocolPrefix + '//' + location.host + path;
    if (socketsByUrl[url]) {
	return socketsByUrl[url];
    } else {
	let s = createWebSocket(path);
	console.log("Creating WebSocket to " + url);
	socketsByUrl[url] = s;
	s.addEventListener("close", (event) => {
	    delete socketsByUrl[url];
	});
	registerSocketLoop(s);
	return s;
    }
}

window.addEventListener("load", (event) => {
    let _ = fetchSocket(location.pathname);
});

window.addEventListener("focus", (event) => {
    let _ = fetchSocket(location.pathname);
});
