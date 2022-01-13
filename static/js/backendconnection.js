let setName = function() {
    fetch("/request?" + new URLSearchParams({query: "name(X)"}),
          {method:"post"}).then(function(response) {
              response.text().then(function(text) {
                  document.getElementById("charname").innerHTML = text;
                  document.getElementById("chartitle").innerHTML = text;
              });
          });
};
setName();
let query = function(q) {
    fetch("/request?" + new URLSearchParams({query: q}),
          {method:"post"}).then(function(response) {
              response.text().then(function(text) {
                  console.log(text);
              });
          });
};
let todo = function() {
    fetch("/request?" + new URLSearchParams({todo: "foo"}),
          {method:"post"}).then(function(response) {
              response.text().then(function(text) {
                  console.log(text);
              });
          });
};
let myFunction = function(clicked_id) {
    if(clicked_id == "todo") {
      fetch("/request?" + new URLSearchParams({todo: "foo"}),
              {method:"post"}).then(function(response) {
                  response.text().then(function(text) {
                    const resultJSON = JSON.parse(text);
                    const resultString = JSON.stringify(resultJSON, null, '\t');
                    document.getElementById("outputtest").innerHTML = resultString;
                    console.log(resultString);
                    console.log(resultJSON['id'])
                  });
                  console.log(clicked_id);
              });
            } else if(clicked_id == "query") {
              const q = document.getElementById("queryText").value; 
              fetch("/request?" + new URLSearchParams({query: q}),
                {method:"post"}).then(function(response) {
                  response.text().then(function(text) {
                    document.getElementById("outputtest").innerHTML = text;
                });
              });
            }
};