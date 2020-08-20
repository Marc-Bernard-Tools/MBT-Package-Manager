/*
 * MBT COMMON JS
 */

/**********************************************************
  Global variables used from outside
 **********************************************************/
/* exported setInitialFocus */
/* exported setInitialFocusWithQuerySelector */
/* exported submitFormById */
/* exported errorStub */
/* exported confirmInitialized */
/* exported perfOut */
/* exported perfLog */
/* exported perfClear */
/* exported enableArrowListNavigation */
/* exported activateLinkHints */
/* exported setKeyBindings */
/* exported preparePatch */
/* exported registerStagePatch */
/* exported toggleRepoListDetail */
/* exported onTagTypeChange */
/* exported getIndocStyleSheet */
/* exported addMarginBottom */
/* exported enumerateTocAllRepos */
/* exported enumerateJumpAllFiles */
/* exported enumerateToolbarActions */
/* exported onDiffCollapse */
/* exported restoreScrollPosition */

/**********************************************************
 * Polyfills
 **********************************************************/

// Bind polyfill (for IE7), taken from https://developer.mozilla.org/
if (!Function.prototype.bind) {
  Function.prototype.bind = function(oThis) {
    if (typeof this !== "function") {
      throw new TypeError("Function.prototype.bind - subject is not callable");
    }

    var aArgs   = Array.prototype.slice.call(arguments, 1),
      fToBind = this,
      fNOP    = function() {},
      fBound  = function() {
        return fToBind.apply(
          this instanceof fNOP
            ? this
            : oThis,
          aArgs.concat(Array.prototype.slice.call(arguments))
        );
      };

    if (this.prototype) {
      fNOP.prototype = this.prototype;
    }
    fBound.prototype = new fNOP();

    return fBound;
  };
}

// String includes polyfill, taken from https://developer.mozilla.org
if (!String.prototype.includes) {
  String.prototype.includes = function(search, start) {
    "use strict";
    if (typeof start !== "number") {
      start = 0;
    }

    if (start + search.length > this.length) {
      return false;
    } else {
      return this.indexOf(search, start) !== -1;
    }
  };
}

// String startsWith polyfill, taken from https://developer.mozilla.org
if (!String.prototype.startsWith) {
  Object.defineProperty(String.prototype, "startsWith", {
    value: function(search, pos) {
      pos = !pos || pos < 0 ? 0 : +pos;
      return this.substring(pos, pos + search.length) === search;
    }
  });
}

/**********************************************************
 * Common functions
 **********************************************************/

// Output text to the debug div
function debugOutput(text, dstID) {
  var stdout       = document.getElementById(dstID || "debug-output");
  var wrapped      = "<p>" + text + "</p>";
  stdout.innerHTML = stdout.innerHTML + wrapped;
}

// Use a pre-created form or create a hidden form
// and submit with sapevent
function submitSapeventForm(params, action, method) {
  var stub_form_id = "form_" + action;
  var form = document.getElementById(stub_form_id);

  if (form === null) {
    form = document.createElement("form");
    form.setAttribute("method", method || "post");
    form.setAttribute("action", "sapevent:" + action);
  }

  for(var key in params) {
    var hiddenField = document.createElement("input");
    hiddenField.setAttribute("type", "hidden");
    hiddenField.setAttribute("name", key);
    hiddenField.setAttribute("value", params[key]);
    form.appendChild(hiddenField);
  }

  if (form.id !== stub_form_id) {
    document.body.appendChild(form);
  }

  form.submit();
}

// Set focus to a control
function setInitialFocus(id) {
  document.getElementById(id).focus();
}

// Set focus to a element with query selector
function setInitialFocusWithQuerySelector(sSelector, bFocusParent) {
  var oSelected = document.querySelector(sSelector);

  if (oSelected) {
    if (bFocusParent) {
      oSelected.parentElement.focus();
    } else {
      oSelected.focus();
    }
  }

}

// Submit an existing form
function submitFormById(id) {
  document.getElementById(id).submit();
}

// JS error stub
function errorStub(event) {
  var element    = event.target || event.srcElement;
  var targetName = element.id || element.name || "???";
  alert("JS Error, please log an issue (@" + targetName + ")");
}

// confirm JS initilization
function confirmInitialized() {
  var errorBanner = document.getElementById("js-error-banner");
  if (errorBanner) {
    errorBanner.style.display = "none";
  }
  debugOutput("js: OK"); // Final final confirmation :)
}

/**********************************************************
 * Performance utils (for debugging)
 **********************************************************/

var gPerf = [];

function perfOut(prefix) {
  var totals = {};
  for (var i = gPerf.length - 1; i >= 0; i--) {
    if (!totals[gPerf[i].name]) totals[gPerf[i].name] = {count: 0, time: 0};
    totals[gPerf[i].name].time  += gPerf[i].time;
    totals[gPerf[i].name].count += 1;
  }

  var keys = Object.keys(totals);
  for (var j = keys.length - 1; j >= 0; j--) {
    console.log(prefix
      + " " + keys[j] + ": "
      + totals[keys[j]].time.toFixed(3) + "ms"
      + " (" + totals[keys[j]].count.toFixed() +")");
  }
}

function perfLog(name, startTime) {
  gPerf.push({name: name, time: window.performance.now() - startTime});
}

function perfClear() {
  gPerf = [];
}

/**********************************************************
 * Other functions
 **********************************************************/

// News announcement
function toggleDisplay(divId) {
  var div = document.getElementById(divId);
  if (div) div.style.display = (div.style.display) ? "" : "none";
}

function KeyNavigation() { }

KeyNavigation.prototype.onkeydown = function(event) {
  if (event.defaultPrevented) return;

  // navigate with arrows through list items and support pressing links with enter and space
  var isHandled = false;
  if (event.key === "Enter" || event.key === "") {
    isHandled = this.onEnterOrSpace();
  } else if (/Down$/.test(event.key)) {
    isHandled = this.onArrowDown();
  } else if (/Up$/.test(event.key)) {
    isHandled = this.onArrowUp();
  } else if (event.key === "Backspace") {
    isHandled = this.onBackspace();
  }

  if (isHandled) event.preventDefault();
};

KeyNavigation.prototype.onEnterOrSpace = function () {
  if (document.activeElement.nodeName !== "A") return;
  var anchor = document.activeElement;

  if (anchor.href.replace(/#$/, "") === document.location.href.replace(/#$/, "")
    && !anchor.onclick
    && anchor.parentElement
    && anchor.parentElement.nodeName === "LI" ) {
    anchor.parentElement.classList.toggle("force-nav-hover");
  } else {
    anchor.click();
  }
  return true;
};

KeyNavigation.prototype.focusListItem = function (li) {
  var anchor = li.firstElementChild;
  if (!anchor || anchor.nodeName !== "A") return false;
  anchor.focus();
  return true;
};

KeyNavigation.prototype.closeDropdown = function (dropdownLi) {
  dropdownLi.classList.remove("force-nav-hover");
  if (dropdownLi.firstElementChild.nodeName === "A") dropdownLi.firstElementChild.focus();
  return true;
};

KeyNavigation.prototype.onBackspace = function () {
  var activeElement = document.activeElement;

  // Detect opened subsequent dropdown
  if (activeElement.nodeName === "A"
    && activeElement.parentElement
    && activeElement.parentElement.nodeName === "LI"
    && activeElement.parentElement.classList.contains("force-nav-hover")) {
    return this.closeDropdown(activeElement.parentElement);
  }

  // Detect opened parent dropdown
  if (activeElement.nodeName === "A"
    && activeElement.parentElement
    && activeElement.parentElement.nodeName === "LI"
    && activeElement.parentElement.parentElement
    && activeElement.parentElement.parentElement.nodeName === "UL"
    && activeElement.parentElement.parentElement.parentElement
    && activeElement.parentElement.parentElement.parentElement.nodeName === "LI"
    && activeElement.parentElement.parentElement.parentElement.classList.contains("force-nav-hover")) {
    return this.closeDropdown(activeElement.parentElement.parentElement.parentElement);
  }
};

KeyNavigation.prototype.onArrowDown = function () {
  var activeElement = document.activeElement;

  // Start of dropdown list: LI > selected A :: UL > LI > A
  if (activeElement.nodeName === "A"
    && activeElement.parentElement
    && activeElement.parentElement.nodeName === "LI"
    && activeElement.parentElement.classList.contains("force-nav-hover") // opened dropdown
    && activeElement.nextElementSibling
    && activeElement.nextElementSibling.nodeName === "UL"
    && activeElement.nextElementSibling.firstElementChild
    && activeElement.nextElementSibling.firstElementChild.nodeName === "LI") {
    return this.focusListItem(activeElement.nextElementSibling.firstElementChild);
  }

  // Next item of dropdown list: ( LI > selected A ) :: LI > A
  if (activeElement.nodeName === "A"
    && activeElement.parentElement
    && activeElement.parentElement.nodeName === "LI"
    && activeElement.parentElement.nextElementSibling
    && activeElement.parentElement.nextElementSibling.nodeName === "LI") {
    return this.focusListItem(activeElement.parentElement.nextElementSibling);
  }
};

KeyNavigation.prototype.onArrowUp = function () {
  var activeElement = document.activeElement;

  // Prev item of dropdown list: ( LI > selected A ) <:: LI > A
  if (activeElement.nodeName === "A"
    && activeElement.parentElement
    && activeElement.parentElement.nodeName === "LI"
    && activeElement.parentElement.previousElementSibling
    && activeElement.parentElement.previousElementSibling.nodeName === "LI") {
    return this.focusListItem(activeElement.parentElement.previousElementSibling);
  }
};

KeyNavigation.prototype.getHandler = function () {
  return this.onkeydown.bind(this);
};

// this functions enables the navigation with arrows through list items (li)
// e.g. in dropdown menus
function enableArrowListNavigation() {
  document.addEventListener("keydown", new KeyNavigation().getHandler());
}

/* LINK HINTS - Vimium like link hints */

function LinkHints(linkHintHotKey){
  this.linkHintHotKey    = linkHintHotKey;
  this.areHintsDisplayed = false;
  this.pendingPath       = ""; // already typed code prefix
  this.hintsMap          = this.deployHintContainers();
  this.activatedDropdown = null;
}

LinkHints.prototype.getHintStartValue = function(targetsCount){
  // e.g. if we have 89 tooltips we start from 10
  //      if we have 90 tooltips we start from 100
  //      if we have 900 tooltips we start from 1000
  var
    baseLength = Math.pow(10, targetsCount.toString().length - 1),
    maxHintStringLength = (targetsCount + baseLength).toString().length;
  return Math.pow(10, maxHintStringLength - 1);
};

LinkHints.prototype.deployHintContainers = function() {

  var hintTargets = document.querySelectorAll("a, input[type='checkbox']");
  var codeCounter = this.getHintStartValue(hintTargets.length);
  var hintsMap    = { first: codeCounter };

  // <span class="link-hint" data-code="123">
  //   <span class="pending">12</span><span>3</span>
  // </span>
  for (var i = 0, N = hintTargets.length; i < N; i++) {
    var hint = {};
    hint.container     = document.createElement("span");
    hint.pendingSpan   = document.createElement("span");
    hint.remainingSpan = document.createElement("span");
    hint.parent        = hintTargets[i];
    hint.code          = codeCounter.toString();

    hint.container.appendChild(hint.pendingSpan);
    hint.container.appendChild(hint.remainingSpan);

    hint.pendingSpan.classList.add("pending");
    hint.container.classList.add("link-hint");
    if (hint.parent.nodeName === "INPUT"){
      hint.container.classList.add("link-hint-input");
    } else {
      hint.container.classList.add("link-hint-a");
    }

    hint.container.classList.add("nodisplay");            // hide by default
    hint.container.dataset.code = codeCounter.toString(); // not really needed, more for debug

    if (hintTargets[i].nodeName === "INPUT") {
      // does not work if inside the input, so appending right after
      hintTargets[i].insertAdjacentElement("afterend", hint.container);
    } else {
      hintTargets[i].appendChild(hint.container);
    }
    hintsMap[codeCounter++] = hint;
  }

  hintsMap.last = codeCounter - 1;
  return hintsMap;
};

LinkHints.prototype.getHandler = function() {
  return this.handleKey.bind(this);
};

LinkHints.prototype.handleKey = function(event){

  if (event.defaultPrevented) {
    return;
  }

  var activeElementType = (document.activeElement && document.activeElement.nodeName) || "";

  // link hints are disabled for input and textareas for obvious reasons.
  // Maybe we must add other types here in the future
  if (event.key === this.linkHintHotKey && activeElementType !== "INPUT" && activeElementType !== "TEXTAREA") {

    // on user hide hints, close an opened dropdown too
    if (this.areHintsDisplayed && this.activatedDropdown) this.closeActivatedDropdown();

    this.pendingPath = "";
    this.displayHints(!this.areHintsDisplayed);

  } else if (this.areHintsDisplayed) {

    // the user tries to reach a hint
    this.pendingPath += event.key;
    var hint = this.hintsMap[this.pendingPath];

    if (hint) { // we are there, we have a fully specified tooltip. Let's activate it
      this.displayHints(false);
      this.hintActivate(hint);
    } else {
      // we are not there yet, but let's filter the link so that only
      // the partially matched are shown
      var visibleHints = this.filterHints();
      if (!visibleHints) {
        this.displayHints(false);
        if (this.activatedDropdown) this.closeActivatedDropdown();
      }
    }
  }
};

LinkHints.prototype.closeActivatedDropdown = function() {
  if (!this.activatedDropdown) return;
  this.activatedDropdown.classList.remove("force-nav-hover");
  this.activatedDropdown = null;
};

LinkHints.prototype.displayHints = function(isActivate) {
  this.areHintsDisplayed = isActivate;
  for (var i = this.hintsMap.first; i <= this.hintsMap.last; i++) {
    var hint = this.hintsMap[i];
    if (isActivate) {
      hint.container.classList.remove("nodisplay");
      hint.pendingSpan.innerText   = "";
      hint.remainingSpan.innerText = hint.code;
    } else {
      hint.container.classList.add("nodisplay");
    }
  }
};

LinkHints.prototype.hintActivate = function (hint) {
  if (hint.parent.nodeName === "A"
    // hint.parent.href doesn't have a # at the end while accessing dropdowns the first time.
    // Seems like a idiosyncrasy of SAPGUI's IE. So let's ignore the last character.
    && ( hint.parent.href.substr(0, hint.parent.href.length - 1) === document.location.href ) // href is #
    && !hint.parent.onclick                         // no handler
    && hint.parent.parentElement && hint.parent.parentElement.nodeName === "LI") {
    // probably it is a dropdown ...
    this.activatedDropdown = hint.parent.parentElement;
    this.activatedDropdown.classList.toggle("force-nav-hover");
    hint.parent.focus();
  } else {
    hint.parent.click();
    if (this.activatedDropdown) this.closeActivatedDropdown();
  }
};

LinkHints.prototype.filterHints = function () {
  var visibleHints = 0;
  for (var i = this.hintsMap.first; i <= this.hintsMap.last; i++) {
    var hint = this.hintsMap[i];
    if (i.toString().startsWith(this.pendingPath)) {
      hint.pendingSpan.innerText   = this.pendingPath;
      hint.remainingSpan.innerText = hint.code.substring(this.pendingPath.length);
      // hint.container.classList.remove("nodisplay"); // for backspace
      visibleHints++;
    } else {
      hint.container.classList.add("nodisplay");
    }
  }
  return visibleHints;
};

function activateLinkHints(linkHintHotKey) {
  if (!linkHintHotKey) return;
  var oLinkHint = new LinkHints(linkHintHotKey);
  document.addEventListener("keypress", oLinkHint.getHandler());
}

/* HOTKEYS */

function Hotkeys(oKeyMap){

  this.oKeyMap = oKeyMap || {};

  // these are the hotkeys provided by the backend
  Object.keys(this.oKeyMap).forEach(function(sKey){

    var action = this.oKeyMap[sKey];

    // add a tooltip/title with the hotkey, currently only sapevents are supported
    [].slice.call(document.querySelectorAll("a[href^='sapevent:" + action + "']")).forEach(function(elAnchor) {
      elAnchor.title = elAnchor.title + " [" + sKey + "]";
    });

    // We replace the actions with callback functions to unify
    // the hotkey execution
    this.oKeyMap[sKey] = function(oEvent) {

      // We have either a js function on this
      if (this[action]) {
        this[action].call(this);
        return;
      }

      // Or a global function
      if (window[action]) {
        window[action].call(this);
        return;
      }

      // Or a SAP event
      var sUiSapEvent = this.getSapEvent(action);
      if (sUiSapEvent) {
        submitSapeventForm({}, sUiSapEvent, "post");
        oEvent.preventDefault();
        return;
      }

    };

  }.bind(this));

}

Hotkeys.prototype.showHotkeys = function() {
  var elHotkeys = document.querySelector("#hotkeys");

  if (elHotkeys) {
    elHotkeys.style.display = (elHotkeys.style.display) ? "" : "none";
  }
};

Hotkeys.prototype.getSapEvent = function(sSapEvent) {

  var fnNormalizeSapEventHref = function(sSapEvent, oSapEvent) {
    if (new RegExp(sSapEvent + "$" ).test(oSapEvent.href)
    || (new RegExp(sSapEvent + "\\?" ).test(oSapEvent.href))) {
      return oSapEvent.href.replace("sapevent:","");
    }
  };

  var aSapEvents = document.querySelectorAll('a[href^="sapevent:' + sSapEvent + '"]');

  var aFilteredAndNormalizedSapEvents =
    [].map.call(aSapEvents, function(oSapEvent){
      return fnNormalizeSapEventHref(sSapEvent, oSapEvent);
    }).filter(function(elem){
      // remove false positives
      return (elem && !elem.includes("sapevent:"));
    });

  return (aFilteredAndNormalizedSapEvents && aFilteredAndNormalizedSapEvents[0]);

};

Hotkeys.prototype.onkeydown = function(oEvent){

  if (oEvent.defaultPrevented) {
    return;
  }

  var activeElementType = ((document.activeElement && document.activeElement.nodeName) || "");

  if (activeElementType === "INPUT" || activeElementType === "TEXTAREA") {
    return;
  }

  var
    sKey = oEvent.key || String.fromCharCode(oEvent.keyCode),
    fnHotkey = this.oKeyMap[sKey];

  if (fnHotkey) {
    fnHotkey.call(this, oEvent);
  }
};

Hotkeys.addHotkeyToHelpSheet = function(key, description) {
  var hotkeysUl = document.querySelector("#hotkeys ul.hotkeys");
  if (!hotkeysUl) return;

  var li              = document.createElement("li");
  var spanId          = document.createElement("span");
  spanId.className    = "key-id";
  spanId.innerText    = key;
  var spanDescr       = document.createElement("span");
  spanDescr.className = "key-descr";
  spanDescr.innerText = description;
  li.appendChild(spanId);
  li.appendChild(spanDescr);

  hotkeysUl.appendChild(li);
};

function setKeyBindings(oKeyMap){

  var oHotkeys = new Hotkeys(oKeyMap);

  document.addEventListener("keypress", oHotkeys.onkeydown.bind(oHotkeys));
  setTimeout(function(){
    var div = document.getElementById("hotkeys-hint");
    if (div) div.style.opacity = 0.2;
  }, 4900);
  setTimeout(function(){ toggleDisplay("hotkeys-hint") }, 5000);
}

/* CTRL+P - COMMAND PALETTE */

// fuzzy match helper
// return non empty marked string in case it fits the filter
// abc + b = a<mark>b</mark>c
function fuzzyMatchAndMark(str, filter){
  var markedStr   = "";
  var filterLower = filter.toLowerCase();
  var strLower    = str.toLowerCase();
  var cur         = 0;

  for (var i = 0; i < filter.length; i++) {
    while (filterLower[i] !== strLower[cur] && cur < str.length) {
      markedStr += str[cur++];
    }
    if (cur === str.length) break;
    markedStr += "<mark>" + str[cur++] + "</mark>";
  }

  var matched = i === filter.length;
  if (matched && cur < str.length) markedStr += str.substring(cur);
  return matched ? markedStr : null;
}

function CommandPalette(commandEnumerator, opts) {
  if (typeof commandEnumerator !== "function") throw Error("commandEnumerator must be a function");
  if (typeof opts !== "object") throw Error("opts must be an object");
  if (typeof opts.toggleKey !== "string" || !opts.toggleKey) throw Error("toggleKey must be a string");
  this.commands = commandEnumerator();
  if (!this.commands) return;
  // this.commands = [{
  //   action:    "sap_event_action_code_with_params"
  //   iconClass: "icon icon_x ..."
  //   title:     "my command X"
  // }, ...];

  if (opts.toggleKey[0] === "^") {
    this.toggleKeyCtrl = true;
    this.toggleKey     = opts.toggleKey.substring(1);
    if (!this.toggleKey) throw Error("Incorrect toggleKey");
  } else {
    this.toggleKeyCtrl = false;
    this.toggleKey     = opts.toggleKey;
  }

  this.hotkeyDescription = opts.hotkeyDescription;
  this.elements = {
    palette: null,
    ul:      null,
    input:   null
  };
  this.selectIndex       = -1; // not selected
  this.filter            = "";
  this.renderAndBindElements();
  this.hookEvents();
  Hotkeys.addHotkeyToHelpSheet(opts.toggleKey, opts.hotkeyDescription);
}

CommandPalette.prototype.hookEvents = function(){
  document.addEventListener("keydown", this.handleToggleKey.bind(this));
  this.elements.input.addEventListener("keyup", this.handleInputKey.bind(this));
  this.elements.ul.addEventListener("click", this.handleUlClick.bind(this));
};

CommandPalette.prototype.renderCommandItem = function(cmd){
  var li = document.createElement("li");
  if (cmd.iconClass) {
    var icon       = document.createElement("i");
    icon.className = cmd.iconClass;
    li.appendChild(icon);
  }
  var titleSpan = document.createElement("span");
  li.appendChild(titleSpan);
  cmd.element   = li;
  cmd.titleSpan = titleSpan;
  return li;
};

CommandPalette.prototype.renderAndBindElements = function(){
  var div           = document.createElement("div");
  div.className     = "cmd-palette";
  div.style.display = "none";
  var input         = document.createElement("input");
  input.placeholder = this.hotkeyDescription;
  var ul            = document.createElement("ul");
  for (var i = 0; i < this.commands.length; i++) ul.appendChild(this.renderCommandItem(this.commands[i]));
  div.appendChild(input);
  div.appendChild(ul);

  this.elements.palette = div;
  this.elements.input   = input;
  this.elements.ul      = ul;
  document.body.appendChild(div);
};

CommandPalette.prototype.handleToggleKey = function(event){
  if (event.key !== this.toggleKey) return;
  if (this.toggleKeyCtrl && !event.ctrlKey) return;
  this.toggleDisplay();
  event.preventDefault();
};

CommandPalette.prototype.handleInputKey = function(event){
  if (event.key === "ArrowUp" || event.key === "Up") {
    this.selectPrev();
  } else if (event.key === "ArrowDown" || event.key === "Down") {
    this.selectNext();
  } else if (event.key === "Enter") {
    this.exec(this.getSelected());
  } else if (event.key === "Backspace" && !this.filter) {
    this.toggleDisplay(false);
  } else if (this.filter !== this.elements.input.value) {
    this.filter = this.elements.input.value;
    this.applyFilter();
    this.selectFirst();
  }
  event.preventDefault();
};

CommandPalette.prototype.applyFilter = function(){
  for (var i = 0; i < this.commands.length; i++) {
    var cmd = this.commands[i];
    if (!this.filter) {
      cmd.element.style.display = "";
      cmd.titleSpan.innerText   = cmd.title;
    } else {
      var matchedTitle = fuzzyMatchAndMark(cmd.title, this.filter);
      if (matchedTitle) {
        cmd.titleSpan.innerHTML   = matchedTitle;
        cmd.element.style.display = "";
      } else {
        cmd.element.style.display = "none";
      }
    }
  }
};

CommandPalette.prototype.applySelectIndex = function(newIndex){
  if (newIndex !== this.selectIndex) {
    if (this.selectIndex >= 0) this.commands[this.selectIndex].element.classList.remove("selected");
    var newCmd = this.commands[newIndex];
    newCmd.element.classList.add("selected");
    this.selectIndex = newIndex;
    this.adjustScrollPosition(newCmd.element);
  }
};

CommandPalette.prototype.selectFirst = function(){
  for (var i = 0; i < this.commands.length; i++) {
    if (this.commands[i].element.style.display === "none") continue; // skip hidden
    this.applySelectIndex(i);
    break;
  }
};

CommandPalette.prototype.selectNext = function(){
  for (var i = this.selectIndex + 1; i < this.commands.length; i++) {
    if (this.commands[i].element.style.display === "none") continue; // skip hidden
    this.applySelectIndex(i);
    break;
  }
};

CommandPalette.prototype.selectPrev = function(){
  for (var i = this.selectIndex - 1; i >= 0; i--) {
    if (this.commands[i].element.style.display === "none") continue; // skip hidden
    this.applySelectIndex(i);
    break;
  }
};

CommandPalette.prototype.getSelected = function(){
  return this.commands[this.selectIndex];
};

CommandPalette.prototype.adjustScrollPosition = function(itemElement){
  var bItem         = itemElement.getBoundingClientRect();
  var bContainer    = this.elements.ul.getBoundingClientRect();
  bItem.top         = Math.round(bItem.top);
  bItem.bottom      = Math.round(bItem.bottom);
  bItem.height      = Math.round(bItem.height);
  bItem.mid         = Math.round(bItem.top + bItem.height / 2);
  bContainer.top    = Math.round(bContainer.top);
  bContainer.bottom = Math.round(bContainer.bottom);

  if ( bItem.mid > bContainer.bottom - 2 ) {
    this.elements.ul.scrollTop += bItem.bottom - bContainer.bottom;
  } else if ( bItem.mid < bContainer.top + 2 ) {
    this.elements.ul.scrollTop += bItem.top - bContainer.top;
  }
};

CommandPalette.prototype.toggleDisplay = function(forceState) {
  var isDisplayed = (this.elements.palette.style.display !== "none");
  var tobeDisplayed = (forceState !== undefined) ? forceState : !isDisplayed;
  this.elements.palette.style.display = tobeDisplayed ? "" : "none";
  if (tobeDisplayed) {
    this.elements.input.value = "";
    this.elements.input.focus();
    this.applyFilter();
    this.selectFirst();
  }
};

CommandPalette.prototype.getCommandByElement = function(element) {
  for (var i = 0; i < this.commands.length; i++) {
    if (this.commands[i].element === element) return this.commands[i];
  }
};

CommandPalette.prototype.handleUlClick = function(event) {
  var element = event.target || event.srcElement;
  if (!element) return;
  if (element.nodeName === "SPAN") element = element.parentNode;
  if (element.nodeName === "I") element = element.parentNode;
  if (element.nodeName !== "LI") return;
  this.exec(this.getCommandByElement(element));
};

CommandPalette.prototype.exec = function(cmd) {
  if (!cmd) return;
  this.toggleDisplay(false);
  if (typeof cmd.action === "function"){
    cmd.action();
  } else {
    submitSapeventForm(null, cmd.action);
  }
};

/* COMMAND ENUMERATORS */

function enumerateTocAllRepos() {
  var root = document.getElementById("toc-all-repos");
  if (!root || root.nodeName !== "UL") return null;

  var items = [];
  for (var i = 0; i < root.children.length; i++) {
    if (root.children[i].nodeName === "LI") items.push(root.children[i]);
  }

  items = items.map(function(listItem) {
    var anchor = listItem.children[0];
    return {
      action:    anchor.href.replace("sapevent:", ""),  // a
      iconClass: anchor.childNodes[0].className,        // i with icon
      title:     anchor.childNodes[1].textContent       // text with repo name
    };
  });

  return items;
}

function enumerateToolbarActions() {

  var items = [];
  function processUL(ulNode, prefix) {
    for (var i = 0; i < ulNode.children.length; i++) {
      var item = ulNode.children[i];
      if (item.nodeName !== "LI") continue; // unexpected node
      if (item.children.length >=2 && item.children[1].nodeName === "UL") {
        // submenu detected
        processUL(item.children[1], item.children[0].innerText);
      } else if (item.firstElementChild && item.firstElementChild.nodeName === "A") {
        var anchor = item.firstElementChild;
        if (anchor.href && anchor.href !== "#") items.push([anchor, prefix]);
      }
    }
  }

  var toolbarRoot = document.getElementById("toolbar-main");
  if (toolbarRoot && toolbarRoot.nodeName === "UL") processUL(toolbarRoot);
  toolbarRoot = document.getElementById("toolbar-repo");
  if (toolbarRoot && toolbarRoot.nodeName === "UL") processUL(toolbarRoot);
  // Add more toolbars ?
  if (items.length === 0) return;

  items = items.map(function(item) {
    var anchor = item[0];
    var prefix = item[1];
    return {
      action:    anchor.href.replace("sapevent:", ""),
      title:     (prefix ? prefix + ": " : "") + anchor.innerText
    };
  });

  return items;
}

function enumerateJumpAllFiles() {
  var root = document.getElementById("jump");
  if (!root || root.nodeName !== "UL") return null;

  return Array
    .prototype.slice.call(root.children)
    .filter(function(elem) { return elem.nodeName === "LI" })
    .map(function(listItem) {
      var title = listItem.children[0].childNodes[0].textContent;
      return {
        action: root.onclick.bind(null, title),
        title:  title
      };});
}

/* SCROLL POSITION */

function saveScrollPosition() {
	if (!window.sessionStorage) { return }
	window.sessionStorage.setItem("scrollTop", document.querySelector("html").scrollTop);
}

function restoreScrollPosition() {
	if (!window.sessionStorage) { return }

	var scrollTop = window.sessionStorage.getItem("scrollTop");
	if (scrollTop) {
		document.querySelector("html").scrollTop = scrollTop;
	}
	// window.sessionStorage.setItem("scrollTop", 0);
	window.sessionStorage.removeItem('scrollTop');
}

document.addEventListener("DOMContentLoaded", function (event) {
	restoreScrollPosition();
});

window.addEventListener("beforeunload", function (e) {
	saveScrollPosition();
});
