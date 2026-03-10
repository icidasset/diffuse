import Output from "~/components/configurator/output/element.js";
import { effect } from "~/common/signal.js";

const output = new Output();

document.body.append(output);

effect(() => {
  const col = output.tracks.collection();
  if (col.state === "loaded") {
    console.log(col.data);
  }
});
