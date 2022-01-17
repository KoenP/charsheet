//functions for the character selector page

//initialize the character selector
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
  
  //set character to the chosen one if button is clicked (in new tab or replace current window)
  async function initChar(clicked_id) {
    const resultJSON = await charList();
    const chosenCharacter = resultJSON[document.getElementById("characterChoice").value];
    const result = await loadChar(chosenCharacter);
    document.getElementById("outputtest").innerHTML = "Loading " + chosenCharacter + " in new tab...";
    if(clicked_id == "newtab") open("http://localhost:8000/sheet");
    if(clicked_id == "replace") window.location.replace("http://localhost:8000/sheet");
  }
  
  //send query in input box to server and put response on page
  async function queryClicked() {
    const q = document.getElementById("queryText").value;
    document.getElementById("outputtest").innerHTML = await query(q);
  }
  

  //send todo request to server and put response on page
  async function todoClicked() {
    document.getElementById("outputtest").innerHTML = await request("todo", {});
  }
  
  //generate the lists for the todo selections
  //TODO: implement unique from, multiple choice, add class that levels as info
  async function generateLists() {
    let resultJSON = await todo();
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
  
  //get chosen options and put them on the page
  //TODO: send choices to server and make a save character button
  async function chooseOption(clicked_value) {
    let resultJSON = JSON.parse(await request("todo", {}));
    console.log(JSON.stringify(resultJSON));
    console.log(clicked_value);
    selectList = document.getElementById(clicked_value + " Select");
    document.getElementById("outputtest").innerHTML = ignoreUniqueFrom(resultJSON[clicked_value].spec)[selectList.value];
  }