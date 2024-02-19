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
/* exported enumerateToolbarActions */

/**********************************************************
 * Polyfills
 **********************************************************/

// Bind polyfill (for IE7), taken from https://developer.mozilla.org/
if (!Function.prototype.bind) {
  Function.prototype.bind = function(oThis) {
    if (typeof this !== "function") {
      throw new TypeError("Function.prototype.bind - subject is not callable");
    }

    var aArgs   = Array.prototype.slice.call(arguments, 1);
    var fToBind = this;
    var fNOP    = function() { };
    var fBound  = function() {
      return fToBind.apply(
        this instanceof fNOP ? this : oThis,
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

// forEach polyfill, taken from https://developer.mozilla.org
// used for querySelectorAll results
if (window.NodeList && !NodeList.prototype.forEach) {
  NodeList.prototype.forEach = Array.prototype.forEach;
}

/**********************************************************
 * Common functions
 **********************************************************/

// Output text to the debug div
function debugOutput(text, dstID) {
  var stdout  = document.getElementById(dstID || "debug-output");
  var wrapped = "<p>" + text + "</p>";

  stdout.innerHTML = stdout.innerHTML + wrapped;
}

// Use a supplied form, a pre-created form or create a hidden form
// and submit with sapevent
function submitSapeventForm(params, action, method, form) {

  function getSapeventPrefix() {
    // Depending on the used browser control and its version, different URL schemes
    // are used which we distinguish here
    if (document.querySelector('a[href*="file:///SAPEVENT:"]')) {
      // Prefix for old (SAPGUI <= 8.00 PL3) chromium based browser control
      return "file:///";
    } else if (document.querySelector('a[href^="sap-cust"]')) {
      // Prefix for new (SAPGUI >= 8.00 PL3 Hotfix 1) chromium based browser control
      return "sap-cust://sap-place-holder/";
    } else {
      return ""; // No prefix for old IE control
    }
  }

  var stub_form_id = "form_" + action;

  form = form
    || document.getElementById(stub_form_id)
    || document.createElement("form");

  form.setAttribute("method", method || "post");
  if (/sapevent/i.test(action)) {
    form.setAttribute("action", action);
  } else {
    form.setAttribute("action", getSapeventPrefix() + "SAPEVENT:" + action);
  }

  for (var key in params) {
    var hiddenField = document.createElement("input");
    hiddenField.setAttribute("type", "hidden");
    hiddenField.setAttribute("name", key);
    hiddenField.setAttribute("value", params[key]);
    form.appendChild(hiddenField);
  }

  var formExistsInDOM = form.id && Boolean(document.querySelector("#" + form.id));

  if (form.id !== stub_form_id && !formExistsInDOM) {
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

// Confirm JS initialization
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
    if (!totals[gPerf[i].name]) totals[gPerf[i].name] = { count: 0, time: 0 };

    totals[gPerf[i].name].time  += gPerf[i].time;
    totals[gPerf[i].name].count += 1;
  }

  var keys = Object.keys(totals);
  for (var j = keys.length - 1; j >= 0; j--) {
    console.log(prefix
      + " " + keys[j] + ": "
      + totals[keys[j]].time.toFixed(3) + "ms"
      + " (" + totals[keys[j]].count.toFixed() + ")");
  }
}

function perfLog(name, startTime) {
  gPerf.push({ name: name, time: window.performance.now() - startTime });
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

/**********************************************************
 * Keyboard Navigation
 **********************************************************/

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

KeyNavigation.prototype.onEnterOrSpace = function() {
  if (document.activeElement.nodeName !== "A") return;
  var anchor = document.activeElement;

  if (anchor.href.replace(/#$/, "") === document.location.href.replace(/#$/, "")
    && !anchor.onclick
    && anchor.parentElement
    && anchor.parentElement.nodeName === "LI") {
    anchor.parentElement.classList.toggle("force-nav-hover");
  } else {
    anchor.click();
  }
  return true;
};

KeyNavigation.prototype.focusListItem = function(li) {
  var anchor = li.firstElementChild;
  if (!anchor || anchor.nodeName !== "A") return false;
  anchor.focus();
  return true;
};

KeyNavigation.prototype.closeDropdown = function(dropdownLi) {
  dropdownLi.classList.remove("force-nav-hover");
  if (dropdownLi.firstElementChild.nodeName === "A") dropdownLi.firstElementChild.focus();
  return true;
};

KeyNavigation.prototype.onBackspace = function() {
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

KeyNavigation.prototype.onArrowDown = function() {
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

KeyNavigation.prototype.onArrowUp = function() {
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

KeyNavigation.prototype.getHandler = function() {
  return this.onkeydown.bind(this);
};

// this functions enables the navigation with arrows through list items (li)
// e.g. in dropdown menus
function enableArrowListNavigation() {
  document.addEventListener("keydown", new KeyNavigation().getHandler());
}

/**********************************************************
 * Link Hints (Vimium-like)
 **********************************************************/

function LinkHints(linkHintHotKey) {
  this.linkHintHotKey    = linkHintHotKey;
  this.areHintsDisplayed = false;
  this.pendingPath       = ""; // already typed code prefix
  this.hintsMap          = this.deployHintContainers();
  this.activatedDropdown = null;
  this.yankModeActive    = false;
}

LinkHints.prototype.getHintStartValue = function(targetsCount) {
  // e.g. if we have 89 tooltips we start from 10
  //      if we have 90 tooltips we start from 100
  //      if we have 900 tooltips we start from 1000
  var
    baseLength          = Math.pow(10, targetsCount.toString().length - 1),
    maxHintStringLength = (targetsCount + baseLength).toString().length;
  return Math.pow(10, maxHintStringLength - 1);
};

LinkHints.prototype.deployHintContainers = function() {

  var hintTargets = document.querySelectorAll("a, input, textarea, i");
  var codeCounter = this.getHintStartValue(hintTargets.length);
  var hintsMap    = { first: codeCounter };

  // <span class="link-hint" data-code="123">
  //   <span class="pending">12</span><span>3</span>
  // </span>
  for (var i = 0, N = hintTargets.length; i < N; i++) {
    // skip hidden fields
    if (hintTargets[i].type === "HIDDEN") {
      continue;
    }

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
    if (hint.parent.nodeName === "INPUT" || hint.parent.nodeName === "TEXTAREA") {
      hint.container.classList.add("link-hint-input");
    } else if (hint.parent.nodeName === "A") {
      hint.container.classList.add("link-hint-a");
    } else if (hint.parent.nodeName === "I" && hint.parent.classList.contains("cursor-pointer")) {
      hint.container.classList.add("link-hint-i");
    } else {
      continue;
    }

    hint.container.classList.add("nodisplay"); // hide by default
    hint.container.dataset.code = codeCounter.toString(); // not really needed, more for debug

    if (hintTargets[i].nodeName === "INPUT" || hintTargets[i].nodeName === "TEXTAREA") {
      // does not work if inside the input node
      if (hintTargets[i].type === "checkbox" || hintTargets[i].type === "radio") {
        if (hintTargets[i].nextElementSibling && hintTargets[i].nextElementSibling.nodeName === "LABEL") {
          // insert at end of label
          hintTargets[i].nextElementSibling.appendChild(hint.container);
        } else {
          // inserting right after
          hintTargets[i].insertAdjacentElement("afterend", hint.container);
        }
      } else {
        // inserting right after
        hintTargets[i].insertAdjacentElement("afterend", hint.container);
      }
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

LinkHints.prototype.handleKey = function(event) {
  if (event.defaultPrevented) {
    return;
  }

  if (event.key === "y") {
    this.yankModeActive = !this.yankModeActive;
  }

  if (event.key === this.linkHintHotKey && Hotkeys.isHotkeyCallPossible()) {

    // on user hide hints, close an opened dropdown too
    if (this.areHintsDisplayed && this.activatedDropdown) this.closeActivatedDropdown();
    if (this.areHintsDisplayed) this.yankModeActive = false;

    this.pendingPath = "";
    this.displayHints(!this.areHintsDisplayed);

  } else if (this.areHintsDisplayed) {

    // the user tries to reach a hint
    this.pendingPath += event.key;

    var hint = this.hintsMap[this.pendingPath];

    if (hint) { // we are there, we have a fully specified tooltip. Let us activate or yank it
      this.displayHints(false);
      event.preventDefault();
      if (this.yankModeActive) {
        submitSapeventForm({ clipboard: hint.parent.firstChild.textContent }, "yank_to_clipboard");
        this.yankModeActive = false;
      } else {
        this.hintActivate(hint);
      }
    } else {
      // we are not there yet, but let us filter the link so that only
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

LinkHints.prototype.hintActivate = function(hint) {
  if (hint.parent.nodeName === "A"
    // hint.parent.href doesn`t have a # at the end while accessing dropdowns the first time.
    // Seems like a idiosyncrasy of SAPGUI`s IE. So let`s ignore the last character.
    && (hint.parent.href.substr(0, hint.parent.href.length - 1) === document.location.href)// href is #
    && !hint.parent.onclick // no handler
    && hint.parent.parentElement && hint.parent.parentElement.nodeName === "LI") {
    // probably it is a dropdown ...
    this.activatedDropdown = hint.parent.parentElement;
    this.activatedDropdown.classList.toggle("force-nav-hover");
    hint.parent.focus();
  } else if (hint.parent.type === "checkbox") {
    this.toggleCheckbox(hint);
  } else if (hint.parent.type === "radio") {
    this.toggleRadioButton(hint);
  } else if (hint.parent.type === "submit") {
    hint.parent.click();
  } else if (hint.parent.nodeName === "INPUT" || hint.parent.nodeName === "TEXTAREA") {
    hint.parent.focus();
  } else {
    hint.parent.click();
    if (this.activatedDropdown) this.closeActivatedDropdown();
  }
};

LinkHints.prototype.toggleCheckbox = function(hint) {
  var checked = hint.parent.checked;
  this.triggerClickHandler(hint.parent.parentElement);
  if (checked === hint.parent.checked) {
    // fallback if no handler is registered
    hint.parent.checked = !hint.parent.checked;
  }
};

LinkHints.prototype.toggleRadioButton = function(hint) {
  this.triggerClickHandler(hint.parent);
};

LinkHints.prototype.triggerClickHandler = function(el) {
  // ensures that onclick handler is executed
  // https://stackoverflow.com/questions/41981509/trigger-an-event-when-a-checkbox-is-changed-programmatically-via-javascript
  var event = document.createEvent("HTMLEvents");
  event.initEvent("click", false, true);
  el.dispatchEvent(event);
};

LinkHints.prototype.filterHints = function() {
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

/**********************************************************
 * Hotkeys
 **********************************************************/

function Hotkeys(oKeyMap) {
  this.oKeyMap = oKeyMap || {};

  // these are the hotkeys provided by the backend
  Object.keys(this.oKeyMap).forEach(function(sKey) {

    var action = this.oKeyMap[sKey];

    // add a tooltip/title with the hotkey, currently only sapevents are supported
    this.getAllSapEventsForSapEventName(action).forEach(function(elAnchor) {
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
      if (window[action] && typeof (window[action]) === "function") {
        window[action].call(this);
        return;
      }

      // Or a SAP event link
      var sUiSapEventHref = this.getSapEventHref(action);
      if (sUiSapEventHref) {
        submitSapeventForm({}, sUiSapEventHref, "post");
        oEvent.preventDefault();
        return;
      }

      // Or a SAP event input
      var sUiSapEventInputAction = this.getSapEventInputAction(action);
      if (sUiSapEventInputAction) {
        submitSapeventForm({}, sUiSapEventInputAction, "post");
        oEvent.preventDefault();
        return;
      }

      // Or a SAP event main form
      var elForm = this.getSapEventForm(action);
      if (elForm) {
        elForm.submit();
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

Hotkeys.prototype.getAllSapEventsForSapEventName = function (sSapEvent) {
  if (/^#+$/.test(sSapEvent)){
    // sSapEvent contains only #. Nothing sensible can be done here
    return [];
  }

  var includesSapEvent = function(text){
    return (text.includes("sapevent") || text.includes("SAPEVENT"));
  };

  return [].slice
    .call(document.querySelectorAll("a[href*="+ sSapEvent +"], input[formaction*="+ sSapEvent+"]"))
    .filter(function (elem) {
      return (elem.nodeName === "A" && includesSapEvent(elem.href)
          || (elem.nodeName === "INPUT" && includesSapEvent(elem.formAction)));
    });
};

Hotkeys.prototype.getSapEventHref = function(sSapEvent) {
  return this.getAllSapEventsForSapEventName(sSapEvent)
    .filter(function(el) {
      // only anchors
      return (!!el.href);
    })
    .map(function(oSapEvent) {
      return oSapEvent.href;
    })
    .filter(this.eliminateSapEventFalsePositives(sSapEvent))
    .pop();
};

Hotkeys.prototype.getSapEventInputAction = function(sSapEvent) {
  return this.getAllSapEventsForSapEventName(sSapEvent)
    .filter(function(el) {
      // input forms
      return (el.type === "submit");
    })
    .map(function(oSapEvent) {
      return oSapEvent.formAction;
    })
    .filter(this.eliminateSapEventFalsePositives(sSapEvent))
    .pop();
};

Hotkeys.prototype.getSapEventForm = function(sSapEvent) {
  return this.getAllSapEventsForSapEventName(sSapEvent)
    .filter(function(el) {
      // forms
      var parentForm = el.parentNode.parentNode.parentNode;
      return (el.type === "submit" && parentForm.nodeName === "FORM");
    })
    .map(function(oSapEvent) {
      return oSapEvent.parentNode.parentNode.parentNode;
    })
    .pop();
};

Hotkeys.prototype.eliminateSapEventFalsePositives = function(sapEvent) {
  return function(sapEventAttr) {
    return sapEventAttr.match(new RegExp("\\b" + sapEvent + "\\b"));
  };
};

Hotkeys.prototype.onkeydown = function(oEvent) {
  if (oEvent.defaultPrevented) {
    return;
  }

  if (!Hotkeys.isHotkeyCallPossible()) {
    return;
  }

  var
    sKey     = oEvent.key || String.fromCharCode(oEvent.keyCode),
    fnHotkey = this.oKeyMap[sKey];

  if (fnHotkey) {
    fnHotkey.call(this, oEvent);
  }
};

Hotkeys.isHotkeyCallPossible = function() {
  var activeElementType     = ((document.activeElement && document.activeElement.nodeName) || "");
  var activeElementReadOnly = ((document.activeElement && document.activeElement.readOnly) || false);

  return (activeElementReadOnly || (activeElementType !== "INPUT" && activeElementType !== "TEXTAREA"));
};

Hotkeys.addHotkeyToHelpSheet = function(key, description) {
  var hotkeysUl = document.querySelector("#hotkeys ul.hotkeys");
  if (!hotkeysUl) return;

  var li        = document.createElement("li");
  var spanId    = document.createElement("span");
  var spanDescr = document.createElement("span");

  spanId.className    = "key-id";
  spanId.innerText    = key;
  spanDescr.className = "key-descr";
  spanDescr.innerText = description;
  li.appendChild(spanId);
  li.appendChild(spanDescr);

  hotkeysUl.appendChild(li);
};

function setKeyBindings(oKeyMap) {
  var oHotkeys = new Hotkeys(oKeyMap);

  document.addEventListener("keypress", oHotkeys.onkeydown.bind(oHotkeys));
  setTimeout(function() {
    var div                     = document.getElementById("hotkeys-hint");
    if  (div) div.style.opacity = 0.2;
  }, 4900);
  setTimeout(function() { toggleDisplay("hotkeys-hint") }, 5000);
}

/**********************************************************
 * Command Palette
 **********************************************************/

// fuzzy match helper
// return non empty marked string in case it fits the filter
// abc + b = a<mark>b</mark>c
function fuzzyMatchAndMark(str, filter) {
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
  return matched ? markedStr: null;
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
  this.elements          = {
    palette: null,
    ul     : null,
    input  : null
  };
  this.selectIndex = -1; // not selected
  this.filter      = "";
  this.renderAndBindElements();
  this.hookEvents();
  Hotkeys.addHotkeyToHelpSheet(opts.toggleKey, opts.hotkeyDescription);

  if (!CommandPalette.instances) {
    CommandPalette.instances = [];
  }
  CommandPalette.instances.push(this);
}

CommandPalette.prototype.hookEvents = function() {
  document.addEventListener("keydown", this.handleToggleKey.bind(this));
  this.elements.input.addEventListener("keyup", this.handleInputKey.bind(this));
  this.elements.ul.addEventListener("click", this.handleUlClick.bind(this));
};

CommandPalette.prototype.renderCommandItem = function(cmd) {
  var li = document.createElement("li");
  if (cmd.iconClass) {
    var icon = document.createElement("i");

    icon.className = cmd.iconClass;
    li.appendChild(icon);
  }
  var titleSpan = document.createElement("span");
  li.appendChild(titleSpan);
  cmd.element   = li;
  cmd.titleSpan = titleSpan;
  return li;
};

CommandPalette.prototype.renderAndBindElements = function() {
  var div   = document.createElement("div");
  var input = document.createElement("input");
  var ul    = document.createElement("ul");

  div.className     = "cmd-palette";
  div.style.display = "none";
  input.placeholder = this.hotkeyDescription;
  for (var i = 0; i < this.commands.length; i++) ul.appendChild(this.renderCommandItem(this.commands[i]));
  div.appendChild(input);
  div.appendChild(ul);

  this.elements.palette = div;
  this.elements.input   = input;
  this.elements.ul      = ul;
  document.body.appendChild(div);
};

CommandPalette.prototype.handleToggleKey = function(event) {
  if (event.key !== this.toggleKey) return;
  if (this.toggleKeyCtrl && !event.ctrlKey) return;
  this.toggleDisplay();
  event.preventDefault();
};

CommandPalette.prototype.handleInputKey = function(event) {
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

CommandPalette.prototype.applyFilter = function() {
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

CommandPalette.prototype.applySelectIndex = function(newIndex) {
  if (newIndex !== this.selectIndex) {
    if (this.selectIndex >= 0) this.commands[this.selectIndex].element.classList.remove("selected");
    var newCmd = this.commands[newIndex];
    newCmd.element.classList.add("selected");
    this.selectIndex = newIndex;
    this.adjustScrollPosition(newCmd.element);
  }
};

CommandPalette.prototype.selectFirst = function() {
  for (var i = 0; i < this.commands.length; i++) {
    if (this.commands[i].element.style.display === "none") continue; // skip hidden
    this.applySelectIndex(i);
    break;
  }
};

CommandPalette.prototype.selectNext = function() {
  for (var i = this.selectIndex + 1; i < this.commands.length; i++) {
    if (this.commands[i].element.style.display === "none") continue; // skip hidden
    this.applySelectIndex(i);
    break;
  }
};

CommandPalette.prototype.selectPrev = function() {
  for (var i = this.selectIndex - 1; i >= 0; i--) {
    if (this.commands[i].element.style.display === "none") continue; // skip hidden
    this.applySelectIndex(i);
    break;
  }
};

CommandPalette.prototype.getSelected = function() {
  return this.commands[this.selectIndex];
};

CommandPalette.prototype.adjustScrollPosition = function(itemElement) {
  var bItem      = itemElement.getBoundingClientRect();
  var bContainer = this.elements.ul.getBoundingClientRect();

  bItem.top         = Math.round(bItem.top);
  bItem.bottom      = Math.round(bItem.bottom);
  bItem.height      = Math.round(bItem.height);
  bItem.mid         = Math.round(bItem.top + bItem.height / 2);
  bContainer.top    = Math.round(bContainer.top);
  bContainer.bottom = Math.round(bContainer.bottom);

  if (bItem.mid > bContainer.bottom - 2) {
    this.elements.ul.scrollTop += bItem.bottom - bContainer.bottom;
  } else if (bItem.mid < bContainer.top + 2) {
    this.elements.ul.scrollTop += bItem.top - bContainer.top;
  }
};

CommandPalette.prototype.toggleDisplay = function(forceState) {
  var isDisplayed   = (this.elements.palette.style.display !== "none");
  var tobeDisplayed = (forceState !== undefined) ? forceState : !isDisplayed;

  if (tobeDisplayed) {
    // auto close other command palettes
    CommandPalette.instances.forEach(function(instance) {
      instance.elements.palette.style.display = "none";
    });
  }

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
  if (typeof cmd.action === "function") {
    cmd.action();
  } else {
    submitSapeventForm(null, cmd.action);
  }
};

// Is any command palette visible?
CommandPalette.isVisible = function() {
  return CommandPalette.instances.reduce(function(result, instance) { return result || instance.elements.palette.style.display !== "none" }, false);
};

/**********************************************************
 * Command Enumerators
 **********************************************************/

function enumerateUiActions() {
  var items = [];
  function processUL(ulNode, prefix) {
    for (var i = 0; i < ulNode.children.length; i++) {
      var item = ulNode.children[i];
      if (item.nodeName !== "LI") continue; // unexpected node
      if (item.children.length >= 2 && item.children[1].nodeName === "UL") {
        // submenu detected
        var menutext = item.children[0].innerText;
        // special treatment for menus without text
        if (!menutext) {
          menutext = item.children[0].getAttribute("title");
        }
        processUL(item.children[1], menutext);
      } else if (item.firstElementChild && item.firstElementChild.nodeName === "A") {
        var anchor = item.firstElementChild;
        if (anchor.href && anchor.href !== "#") items.push([anchor, prefix]);
      }
    }
  }

  // toolbars
  [].slice.call(document.querySelectorAll(".nav-container > ul[id*=toolbar]"))
    .filter(function(toolbar) {
      return (toolbar && toolbar.nodeName === "UL");
    }).forEach(function(toolbar) {
      processUL(toolbar);
    });

  items = items.map(function(item) {
    var action = "";
    var anchor = item[0];
    if (anchor.href.includes("#")) {
      action = function() {
        anchor.click();
      };
    } else {
      action = anchor.href.replace("sapevent:", "");
    }
    var prefix = item[1];
    return {
      action: action,
      title : (prefix ? prefix + ": " : "") + anchor.innerText.trim()
    };
  });

  // forms
  [].slice.call(document.querySelectorAll("input[type='submit']"))
    .forEach(function(input) {
      items.push({
        action: function() {
          if (input.form.action.includes(input.formAction) || input.classList.contains("main")) {
            input.form.submit();
          } else {
            submitSapeventForm({}, input.formAction, "post", input.form);
          }
        },
        title: (input.value === "Submit Query" ? input.title : input.value + " " + input.title.replace(/\[.*\]/, ""))
      });
    });

  // radio buttons
  [].slice.call(document.querySelectorAll("input[type='radio']"))
    .forEach(function(input) {
      items.push({
        action: function() {
          input.click();
        },
        title: document.querySelector("label[for='" + input.id + "']").textContent
      });
    });

  // others:
  // - links inside forms
  // - label links
  // - command links
  // - other header links
  [].slice.call(document.querySelectorAll("form a, a.command, #header ul:not([id*='toolbar']) a"))
    .filter(function(anchor) {
      return !!anchor.title || !!anchor.text;
    }).forEach(function(anchor) {
      items.push({
        action: function() {
          anchor.click();
        },
        title: (function() {
          var result = anchor.title + anchor.text;
          if (anchor.href.includes("label")) {
            result = "Label: " + result;
          }
          return result.trim();
        })()
      });
    });

  return items;
}

/**********************************************************
 * Scroll Position Saver
 **********************************************************/

function getPageTitle(){
  var pageTitle = document.getElementsByClassName('title')[0].innerHTML;
  pageTitle = pageTitle.replace(/\<|>|\"|\'/g, '');
  pageTitle = pageTitle.replace(/\s+/g, '');
  if (pageTitle == '&nbsp;') pageTitle = 'Main';
  return pageTitle;
}

document.addEventListener("DOMContentLoaded", function (event) {
  if (!window.sessionStorage) { return }
  var scrollItem = 'scrollPos-' + getPageTitle();
  var scrollPos = window.sessionStorage.getItem(scrollItem);
  if (scrollPos) {
    window.scrollTo(0, scrollPos);
    window.sessionStorage.removeItem(scrollItem);
  }
});

window.addEventListener("beforeunload", function (event) {
  if (!window.sessionStorage) { return }
  var scrollItem = 'scrollPos-' + getPageTitle();
  var scrollPos = document.querySelector("html").scrollTop;
  window.sessionStorage.setItem(scrollItem, scrollPos);
});

/**********************************************************
 * Sticky Header
 **********************************************************/

/* https://www.w3schools.com/howto/howto_js_navbar_sticky.asp */
/* Note: We have to use JS since IE does not support CSS position:sticky */

// When the user scrolls the page, execute toggleSticky
window.onscroll = function() { toggleSticky() };

// Add the sticky class to the navbar when you reach its scroll position.
// Remove "sticky" when you leave the scroll position
function toggleSticky() {
  var body   = document.getElementsByTagName("body")[0];
  var header = document.getElementById("header");
  var sticky = header.offsetTop;

  var stickyClass = "sticky";
  if (body.classList.contains("full_width")) {
    stickyClass = "sticky_full_width";
  }

  if (window.pageYOffset >= sticky) {
    header.classList.add(stickyClass);
  } else {
    header.classList.remove(stickyClass);
  }
}

/**********************************************************
 * Browser Control
 **********************************************************/

// Toggle display of warning message when using Edge (based on Chromium) browser control
// Todo: Remove once https://github.com/abapGit/abapGit/issues/4841 is fixed
function toggleBrowserControlWarning(){
  if (!navigator.userAgent.includes("Edg")){
    var elBrowserControlWarning = document.getElementById("browser-control-warning");
    if (elBrowserControlWarning) {
      elBrowserControlWarning.style.display = "none";
    }
  }
}

// Output type of HTML control in the abapGit footer
function displayBrowserControlFooter() {
  var out = document.getElementById("browser-control-footer");
  out.innerHTML = " - " + ( navigator.userAgent.includes("Edg") ? "Edge" : "IE"  );
}
