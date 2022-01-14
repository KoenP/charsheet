var todoString;

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
                    todoString = resultString;
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

let myFunction2 = function(clicked_id) {
  request({todo: "foo"},
    function(resultJSON) {
      const resultString = JSON.stringify(resultJSON, null, '\t');
      document.getElementById("outputtest").innerHTML = resultString;
      todoString = resultString;
    }
  )
};

let request = function(params, reactToJSON) {
  ask("/request?", "post", params,
  function(response) {
    response.text()
    .then(function(text) {
      const resultJSON = JSON.parse(text);
      reactToJSON(resultJSON);
    });
  });
};

let ask = function(path, method, params, reactToResponse) {
  fetch(path + new URLSearchParams(params), {method:method})
  .then(function(response) {reactToResponse(response);});
};