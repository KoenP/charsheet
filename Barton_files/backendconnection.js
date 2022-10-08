//functions to connect with the backend

//send any http request to backend returns the response as a promise object
async function ask (path, method, params) {
  return await fetch(path + new URLSearchParams(params), {method:method});
}

//send an http post request starting with /request/ followed by the rest of the path returns the response as a string (promise)
async function request(path, params) {
  return (await ask("/request/" + path + "?", "post", params)).text();
}

//like request, but also parses the response as JSON.
async function requestJson(path, params) {
    return JSON.parse(await request(path, params));
}

//send an http request with a query returns the response to the query as a string (promise)
async function query(query) {
  return (await ask("/request/query?", "post", {q: query})).text();
}

//send an http todo request returns a JSON object todo list (promise)
async function todo() {
  return JSON.parse(await request("todo", {}));
}

//send an http list characters request returns a JSON object of possible characters (promise)
async function charList() {
  return JSON.parse(await request("list_characters", {}));
}

//send a query for the name of the current character returns the response as a string (promise)
async function getName() {
  return await query("name(X)");
}

//load given character returns a promise object
async function loadChar(n) {
  return await ask("/request/load_character?", "post", {name: n}); 
}

//create new character with given name
async function newChar() {
  const name = document.getElementById("newcharname").value;
  return await request("new_character", {name: name}); 
}

//function can be used to turn a JSON.spec into JSON.spec.unique_from.spec if necesarry
function ignoreUniqueFrom(spec) {
  if (spec.unique_from) {return spec.unique_from.spec;}
  else {return spec;};
}
