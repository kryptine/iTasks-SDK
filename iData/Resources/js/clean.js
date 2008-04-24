// Script to handle iTask's client architecture.
// It handles everything using Ajax and Sapl
// MJP + JMJ + BL 2007-2008

// version 1.1

// The following symbols are used by Clean and Sapl to separate the different types of information;

var Sapl_ToServer_Separator		= "#0#";				// separator between boolean value and page update response
var State_FormList_Separator	= "##;";				// separator between state info and list of form info
var FormName_Content_Separator	= "###";				// separator between name of form and the contents of a form element
var FormElem_Separator			= "#;;";				// separator between form elements in form list


// Global variables
var xmlHttp		= undefined;							// The xml/http object
var saplApplet	= undefined;							// The sapl applet

var use_ajax	= false;								// Is ajax enabled
var use_sapl	= false;								// Is sapl enabled

var lock 		= false;								// asynchronous submitting of information is not allowed
var viasaple	= false;

//Attach the client-side initialization handler
window.onload = loadPage;

function loadPage() {
	//Reset focus if neccessary
	resetFocus();
	
	//Attach focus handler to all inputs
	attachFocusHandlers();

  	//Detect and initialize sapl
  	saplApplet = document.getElementById("SA");
  	if(saplApplet != undefined) {
  		saplApplet.readsaplfromserver(getAppName());
  		use_sapl = true;
  	} else {
  		use_sapl = false;
  	}
  	
  	//Detect and initialize ajax
  	ajaxoption = document.getElementById("OPT-ajax");
  	if(ajaxoption != undefined) {
  		if(ajaxoption.innerHTML == "true") {
  			//Initialize the xml/httml object
  			initAjax();
    		
    		//Load the start page
    		ajaxInitPage();
    		
  			use_ajax = true;
  		} else {
  			use_ajax = false;
  		}
  	} else {
  		use_ajax = false;
  	}
}

function initAjax() {
	//Initialize the xml/httml object
  	try	{								// Firefox, Opera 8.0+, Safari
		xmlHttp = new XMLHttpRequest();
	} catch (e) {
		try {							// Internet Explorer
			xmlHttp=new ActiveXObject("Msxml2.XMLHTTP");
		} catch (e) {
			xmlHttp=new ActiveXObject("Microsoft.XMLHTTP");
    	}
    }
}

//Resets the focus to the previously selected input
function resetFocus() {
	var focusid = getFocus();
	if(focusid != "")
	{
		var inp = document.getElementById(focusid); 
		if (inp != undefined)
		{
			inp.focus();
		}
	}
}
//Attaches the setFocus handler to each input
function attachFocusHandlers() {

	var tagnames = ["input","select","textarea"];
	
	for(var i = 0; i < tagnames.length; i++) {
	
		var elems = document.getElementsByTagName(tagnames[i]);
		for(var j = 0; j < elems.length; j++) {
			elems[j].onfocus = setFocus
		}
	}
}

//Getters for the state data in the page
function getAppName () {
	return document.getElementById("AN").innerHTML;
}
function getGlobalState () {
	return document.getElementById("GS").innerHTML;
}
function getFocus() {
	return document.getElementById("FS").innerHTML;
}

//Setters for the state data in the page
function setGlobalState (state) {
	document.getElementById("GS").innerHTML = state;
}
function setFocus() {
	document.getElementById("FS").innerHTML = this.id;
}

//Catches a submit of a form
function catchSubmit(form) {
	sendForm(form.id, false); //Send the form to the server (caught submits are never handled on the client)
	return false;
}

//Attaches focus and state information to a form before sending it to clean.
function addState(form) {
	var state = document.createElement('input');
	state.type = 'hidden';
	state.name = 'GS';
	state.value = getGlobalState();
	form.appendChild(state);

	var focus = document.createElement('input');
	focus.type = 'hidden';
	focus.name = 'FS';
	focus.value = getFocus();
	form.appendChild(focus);

	return true;
}

//Sends the information in a form to the server/client handler
function sendForm(formid, onclient) {
	
	var form = document.getElementById(formid);
	
	if(form == undefined) {
		return;
	}
	
	//Add state and focus information to the form
	addState(form);
	
	//If onclient is true, we try to handle the update on the client first
	if(onclient && use_sapl) {
		if(saplSendForm(form)) { //If handling by sapl succeeds, we are done.
			return;				 //When sapl fails we try sending via ajax or submit.
		}
	}

	//Send the form to the server. Either via ajax or with a submit.
	if(use_ajax) {
		ajaxSendForm(form);
	} else {
		submitSendForm(form);
	}
}

//Send an update to clean
//elem		:: the element that triggered the event
//triplet	:: the encoded triplet information
//isaction	:: is the change an action. (e.g. a button is pressed)
//issubmit	:: is the change part of an iData in Submit mode 
//onclient	:: Bool - Should the update be handled by the client			
function toClean(elem, triplet, isaction, issubmit, onclient) {
	if(lock) {
		return;
	}
	
	var form = getForm(elem);
	
	if(isaction) {
		var action = document.createElement('input');
		action.type = 'hidden';
		action.name = triplet;
		action.value = '';
		
		form.appendChild(action);
	}
	
	if(isaction || (!issubmit)) {
		//Delay the submission a little to allow the focus to change first
		//Not the most beautiful solution, but a practical one.
		setTimeout("sendForm('" + form.id + "'," + (onclient ? "true" : "false") + ")",10);
	}
}

//Finds the parent form of an input element		
function getForm(elem)
{
	var parent = elem;
	while (parent.nodeName != 'FORM')
	{
		parent = parent.parentNode;
	}
	return parent;
}

//Sends a form by submitting it
function submitSendForm(form) {


	//Always send the values of checkboxes
	var inputs = form.getElementsByTagName('input');
	for(var i = 0; i < inputs.length; i++) {		
		if(inputs[i].type == "checkbox" && ! inputs[i].checked) {
			//Create a dummy input with the same name and empty value and send this input instead.
			var dummy = document.createElement('input');
			dummy.value = "";
			dummy.type = "hidden";
			dummy.name = inputs[i].name;
			
			//Prevent the input from being sent	
			inputs[i].name = undefined;
			
			//Add the dummy to the form
			form.appendChild(dummy);
		}
	}
	form.submit();
}


//Multipart/Mime encode a form.
function multipartEncodeForm(form) {
	var boundary = genBoundary();
	var values = getFormValues(form);
	
	var msg = "";
	for(var i = 0; i < values.length; i++ ) {
		msg += "\r\n--" + boundary + "\r\n" + "Content-Disposition: form-data; name=\"" + values[i][0] + "\"\r\n\r\n";
		msg += values[i][1];
	}
	msg += "\r\n--" + boundary + "--\r\n";
	
	return [boundary, msg];
}

//Generates a unique string that is used as boundary in a multipart message
function genBoundary() {
	var boundary = "----------";
	for(var i = 0; i < 30; i++ ) {
		boundary += String.fromCharCode(97 + Math.floor(Math.random() * 26));
	}
	return boundary;
}

//Url encode form
function urlEncodeForm(form) {
	var values = getFormValues(form);
	var msg = "";
	
	for(var i = 0; i < values.length; i++) {
		if(i > 0) {
			msg += "&";
		}
		msg += values[i][0] + "=" + values[i][1];
	}
	return msg;
}

//Retrieves the values stored in a form as name, value pairs.
function getFormValues(form) {
	var values = Array();
	
	//Add all <input ...> tag values
	var inputs = form.getElementsByTagName('input');
	for(var i = 0; i < inputs.length; i++) {
		if(inputs[i].disabled) {
			continue;
		}
		//Buttons
		if(inputs[i].type == "button" || inputs[i].type == "submit") {
			continue;
		}
		//Checkboxes
		if(inputs[i].type == "checkbox") {
			if(inputs[i].checked) {
				values[values.length] = [inputs[i].name, inputs[i].value];
			} else {
				values[values.length] = [inputs[i].name, ""];
			}
			continue;
		}
		//Radio inputs
		if(inputs[i].type == "radio") {
			if(inputs[i].checked) {
				values[values.length] = [inputs[i].name, inputs[i].value];
			}
			continue;
		}
		//Other inputs
		values[values.length] = [inputs[i].name, inputs[i].value];
	}
	//Add all <textarea> tag values
	var textareas = form.getElementsByTagName('textarea');
	for(var i = 0; i < textareas.length; i++) {
		values[values.length] = [textareas[i].name, textareas[i].value];
	}
	
	//Add all <select> tag values
	var selects = form.getElementsByTagName('select');
	for(var i = 0; i < selects.length; i++) {
		values[values.length] = [selects[i].name, selects[i].options[selects[i].selectedIndex].value];
	}
	
	return values;	
}

//Get the initial page from the server
function ajaxInitPage() {
	lock = true;
	
	xmlHttp.onreadystatechange = ajaxCallback;
	xmlHttp.open("GET", "/" + getAppName() + "_ajax"  ,true);
	xmlHttp.send(null);
}

//Send form to server using ajax
function ajaxSendForm(form) {
	if(lock) {
		return;
	}
	lock = true;

	//Encode the form
	var msg = multipartEncodeForm(form);

	//Re-initialize ajax (MAY NOT BE NECCESARY. REQUIRES ADDITIONAL TESTING)
	initAjax();
	
	//Send request
	xmlHttp.open("POST", "/" + getAppName() + "_ajax" ,true);	
  	xmlHttp.setRequestHeader("Content-Type", "multipart/form-data; boundary=" + msg[0]);
	xmlHttp.setRequestHeader("Content-Length",msg[1].length);
	xmlHttp.setRequestHeader("Connection", "close");

	xmlHttp.onreadystatechange = ajaxCallback;
	xmlHttp.send(msg[1]);
	
	//Show a short status message
	document.getElementById("iTaskInfo").innerHTML = "<font color=\"yellow\"><b>Server is handling request...</b><hr/>";
}

function ajaxCallback() {

	if (xmlHttp.readyState == 4) {
		
		//Update the page
		splitResponseAndUpdate(xmlHttp.responseText,true);

		//Reset the focus
		resetFocus();
		//Attach the focus event handlers
		attachFocusHandlers();	
		
		lock = false;
 	}
}

//Try to handle the update on client in sapl interpreter
function saplSendForm(form) {
	if(lock) {
		return;
	}
	lock = true;

  	document.getElementById("iTaskInfo").innerHTML = "<font color=\"yellow\"><b>Client is handling request...</b></font><hr/>";

	
	//Encode the form and create an HTTP request message
	var msg = multipartEncodeForm(form);
	var req = "";
	
	req += "POST /" + getAppName() + "_ajax HTTP/1.0\r\n";
	req += "Content-Type: multipart/form-data; boundary=" + msg[0] + "\r\n";
	req += "Content-Length: " + msg[1].length + "\r\n\r\n";
	req += msg[1];
	
	saplApplet.setInput(req);
	saplApplet.evalExpression("start 42");
	
	var rsp = saplApplet.getOutput();

	if(rsp == null) {
		lock = false;
		return false;
	}

	if(rsp.substring(0,5) == "ABORT") {
		lock = false;
		return false;
	}

	var parts = rsp.split(Sapl_ToServer_Separator);

	if( parts.length != 2) {
		lock = false;
		return false;
	}
	
	if( parts[0] == "True") {
		lock = false;
		return false;
	}

	splitResponseAndUpdate(parts[1], false);

	lock = false;
	return true;
}

//Helper functions for ajax and sapl, which update the contents of the page
function splitResponseAndUpdate(response, fromserver) {
	var parts = response.split(State_FormList_Separator);
		
	setGlobalState(parts[0]);
	
	updateDivs(parts[1].split(FormElem_Separator), fromserver);
}

function updateDivs(divcodes, fromserver) {
	
	
	for (var i=0; i < divcodes.length ; i++) {
		
	 	var thisupdate     	= divcodes[i].split(FormName_Content_Separator);
		var codename		= thisupdate[0];
		var theCode			= document.getElementById(codename);
		if (theCode != undefined) {	// the code does not need to be on the page !!!
			theCode.innerHTML	= thisupdate[1];
		}
	}
}




