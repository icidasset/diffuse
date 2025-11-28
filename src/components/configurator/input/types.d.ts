export type AdditionalActions = {
  configure: (args: { ports: { [S in string]: MessagePort } }) => void;
};
