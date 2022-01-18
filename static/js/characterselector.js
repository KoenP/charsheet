//functions for the character selector page

//initialize the character selector
async function initPage() {
    const charJSON =  await charList();
    let menu = document.getElementById("characterChoice")
    for(let i = 0; i < charJSON.list.length; i++) {
      var option = document.createElement("option");
      option.text = charJSON.list[i];
      option.value = option.text;
      menu.add(option);
    }
    if (charJSON.current) {
      console.log(charJSON.current);
      menu.value = charJSON.current;
    }
    else {
      var option = document.createElement("option");
      option.text = "choose your character";
      option.selected = true;
      option.hidden = true;
      option.disabled = true;
      menu.add(option);
    }
  }
  initPage();
  
  //set character to the chosen one if button is clicked (in new tab or replace current window)
  async function initChar(clicked_id) {
    // const resultJSON = await charList();
    //   console.log(resultJSON);
    // const chosenCharacter = resultJSON.list[document.getElementById("characterChoice").value];
    // console.log(chosenCharacter);
    const result = await loadChar(document.getElementById("characterChoice").value);
    // document.getElementById("outputtest").innerHTML = "Loading " + chosenCharacter + " in new tab...";
    // if(clicked_id == "newtab") open("http://localhost:8000/sheet.html");
    // if(clicked_id == "replace") window.open("http://localhost:8000/sheet.html", "_self");
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
