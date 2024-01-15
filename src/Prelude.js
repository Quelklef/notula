
export const dTrace = a => r => {
  console.log(a);
  return r;
};

export const dTraceWith = label => a => {
  console.log(label, a);
  return a;
};

