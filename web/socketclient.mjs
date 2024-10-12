function createWebSocket(path) {
  var protocolPrefix = (window.location.protocol === 'https:') ? 'wss:' : 'ws:';
  return new WebSocket(protocolPrefix + '//' + location.host + path);
}

var ws = createWebSocket(location.pathname);
	
ws.addEventListener("open", (event) => {
  const init_msg = JSON.stringify({
    event: "initialize",
    message: "Hello webpdf-hs!"
  });
  ws.send(init_msg);
});

ws.addEventListener("message", (event) => {
  var msg = JSON.parse(event.data);
  if (msg.event === "reload") {
    console.log("Received reload notice from WebSocket.");
    let new_url = msg.message + "?t=" + Date.now();
    PDFViewerApplication.open({
      url: new_url
    }); // PDFViewerApplication is in /web/viewer.mjs
  } else {
    console.log("Received socket event.")
    console.log(msg)
  }
});

ws.addEventListener("close", (event) => {
  alert("WebSocket connection to host is closed.")
});

window.addEventListener("beforeunload", (event) => {
  ws.close();
})