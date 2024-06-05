(function () {
  onmessage = function (event) {
    let url = event.data;
    let filename = url.substring(url.lastIndexOf('/')+1);
    downloadFile(url)
      .then(function (fileData) {
        postMessage({blob: fileData, name: filename});
      })
      .catch(function (error) {
        postMessage({ error: error.message });
      });
  };

  function downloadFile(url) {
    return fetch(url)
      .then(function (response) {
        if (!response.ok) {
          throw new Error('Network response was not ok');
        }
        return response.blob();
      })
  }
}())