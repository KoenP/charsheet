async function ask (path, method, params) {
  return await fetch(path + new URLSearchParams(params, {method:method}));
}

async function request(path, params) {
  return (await ask("/request/" + path + "?", "post", params)).text();
}

async function query(query) {
  return (await ask("/request/query?", "post", {q: query})).text();
}

async function todo() {
  return JSON.parse(await request("todo", {}));
}

async function charList() {
  return JSON.parse(await request("list_characters", {}));
}

async function getName() {
  return await query("name(X)");
}

async function loadChar(n) {
  ask("/request/load_character?", "post", {name: n}); 
}

async function initPage() {
  const charJSON =  await charList();
  for(let i = 0; i < charJSON.length; i++) {
    var option = document.createElement("option");
    option.text = charJSON[i];
    option.value = i;
    document.getElementById("characterChoice").add(option);
  }
}
initPage();

async function initChar() {
  const charJSON = await charList();
  console.log(JSON.stringify(charJSON, null, '\t'));
  await loadChar(charJSON[document.getElementById("characterChoice").value]);
  console.log(charJSON[document.getElementById("characterChoice").value])
  document.getElementById("outputtest").innerHTML = await getName();
}

async function queryClicked() {
  const q = document.getElementById("queryText").value;
  document.getElementById("outputtest").innerHTML = await query(q);
}

async function todoClicked() {
  document.getElementById("outputtest").innerHTML = await request("todo", {});
}

async function generateLists() {
  let resultJSON = todo();
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
}

async function chooseOption(clicked_value) {
  let resultJSON = JSON.parse(await request("todo", {}));
  console.log(JSON.stringify(resultJSON));
  console.log(clicked_value);
  selectList = document.getElementById(clicked_value + " Select");
  document.getElementById("outputtest").innerHTML = ignoreUniqueFrom(resultJSON[clicked_value].spec)[selectList.value];
}

function ignoreUniqueFrom(spec) {
  if (spec.unique_from) {return spec.unique_from.spec;}
  else {return spec;};
}