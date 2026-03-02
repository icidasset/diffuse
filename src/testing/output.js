import Output from "~/components/configurator/output/element.js";
import { effect } from "~/common/signal.js";

const output = new Output();

document.body.append(output);

effect(() => {
  console.log(output.tracks.state());
});

effect(() => {
  console.log(output.tracks.collection());
});
