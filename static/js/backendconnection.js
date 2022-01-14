let setName = function() {
  fetch("/request/query?" + new URLSearchParams({q: "name(X)"}),
        {method:"post"}).then(function(response) {
            response.text().then(function(text) {
                document.getElementById("charname").innerHTML = text;
                document.getElementById("chartitle").innerHTML = text;
            });
        });
};

setName();

let query = function(query, continueWithResult) {
  ask("/request/query?", "post", {q: query},
  function(response) {
    response.text()
    .then(continueWithResult);
  });
};


//let query = function(q) {
//    fetch("/request?" + new URLSearchParams({query: q}),
//          {method:"post"}).then(function(response) {
//              response.text().then(function(text) {
//                  console.log(text);
//              });
//          });
//};
//let todo = function() {
//    fetch("/request?" + new URLSearchParams({todo: "foo"}),
//          {method:"post"}).then(function(response) {
//              response.text().then(function(text) {
//                  console.log(text);
//              });
//          });
//};
let myFunction = function(clicked_id) {
    const q = document.getElementById("queryText").value; 
    query(q, function(text) {document.getElementById("outputtest").innerHTML = text;});
};

let myFunction2 = function() {
    request("todo", {}, 
      function(resultJSON) {
        const resultString = JSON.stringify(resultJSON, null, '\t');
        document.getElementById("outputtest").innerHTML = resultString;
        if(resultJSON[2].spec[0] == undefined) console.log(resultJSON[2].spec.unique_from.spec[0]);
        else console.log(resultJSON[2].spec[0]);
    });
};

let request = function(path, params, reactToJSON) {
  ask("/request/" + path + "?", "post", params,
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

let generateLists = function() {
  request("todo", {}, 
  function(resultJSON){
    parent = document.getElementById("outputdiv");
    for(let i = 0; i < resultJSON.length; i++) {
      var newLabel = document.createElement("p");
      newLabel.innerHTML = resultJSON[i].id;
      var newSelect = document.createElement("select");
      newSelect.id = i + " Select";
      var newButton = document.createElement("button");
      newButton.innerHTML = "Choose";
      newButton.value = i;
      newButton.setAttribute("onclick", "chooseOption(this.value)");
      parent.insertBefore(newButton, document.getElementById("outputtest"));
      parent.insertBefore(newSelect, newButton);
      parent.insertBefore(newLabel, newSelect);
      specList = ignoreUniqueFrom(resultJSON[i].spec);
      for(let j = 0; j < specList.length; j++) {
        var option = document.createElement("option");
        option.text = ignoreUniqueFrom(resultJSON[i].spec)[j];
        option.value = j;
        newSelect.add(option);
      }
    }
  });
};

let chooseOption = function(clicked_value) {
  request("todo", {},
  function(resultJSON) {
    console.log(JSON.stringify(resultJSON));
    console.log(clicked_value);
    selectList = document.getElementById(clicked_value + " Select");
    document.getElementById("outputtest").innerHTML = ignoreUniqueFrom(resultJSON[clicked_value].spec)[selectList.value];
  });
};

let ignoreUniqueFrom = function(spec) {
  if (spec.unique_from) {return spec.unique_from.spec;}
  else {return spec;};
};
