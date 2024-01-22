
export const getTargetValue =
evt => {
  return evt.target.value;
}

// Modified from https://stackoverflow.com/a/33928558
export const copyToClipboard =
text => () => {
  if(window.clipboardData&&window.clipboardData.setData){return window.clipboardData.setData("Text",text)}else if(document.queryCommandSupported&&document.queryCommandSupported("copy")){var textarea=document.createElement("textarea");textarea.textContent=text;textarea.style.position="fixed";document.body.appendChild(textarea);textarea.select();try{return document.execCommand("copy")}catch(ex){console.warn("Copy to clipboard failed.",ex);return prompt("Copy to clipboard: Ctrl+C, Enter",text)}finally{document.body.removeChild(textarea)}}
}

export const readQueryParams =
() => {
  return Array.from(new URLSearchParams(window.location.search).entries());
};

export const writeQueryParams =
kvs => () => {
  writeQueryParamsDebounced(kvs);
};

const writeQueryParamsDebounced = jsDebounce(200, kvs => {
  const qp = new URLSearchParams();
  for (const [k, v] of kvs) qp.append(k, v);
  history.replaceState(null, '', window.location.pathname + '?' + qp.toString());
});

function jsDebounce(delayMs, f) {
  let timeoutId = 0;
  return (...args) => {
    clearTimeout(timeoutId);
    timeoutId = setTimeout(() => f(...args), delayMs);
  };
}

export const debounce =
delayMs => f => () => {
  return jsDebounce(delayMs, f);
};
