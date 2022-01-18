
//tried some stuff but it is not working yet
//TODO copy sheet html to page and dynamically fill it with javascript DOM?

window.addEventListener("DOMContentLoaded", initPage());
function sleep(milliseconds) {
    const date = Date.now();
    let currentDate = null;
    do {
      currentDate = Date.now();
    } while (currentDate - date < milliseconds);
  }

  function initPage() {
    let newButton = document.createElement("button");
    newButton.id = "back";
    newButton.innerHTML = "Back to character selector";
    //var parent = document.getElementsByClassName("container");
    //var header = document.querySelector("header");
    //parent[0].insertBefore(newButton, header);
    let elementToAddBefore = document.getElementById("summary");
    document.body.insertBefore(newButton, elementToAddBefore);
    console.log("Hello World!");
  }