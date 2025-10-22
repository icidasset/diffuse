import * as Audio from "@component/engine/audio/element.js";
import * as Queue from "@component/engine/queue/element.js";
import * as Metadata from "@component/processor/metadata/element.js";

import { component } from "@common/element.js";

const audio = component(Audio);
const queue = component(Queue);
const metadata = component(Metadata);

const url =
  "https://archive.org/download/deathofsalesmans00mill/01_Side_1_Death_of_a_salesman_-_Introduction_Act_1__Part_1.mp3";

// const resp = await metadata.supply({
//   urls: { get: url, head: url }
// })

// console.log(resp)

audio.supply({
  audio: [
    {
      id: "test",
      isPreload: false,
      url: url,
    },
  ],
});

// effect(() => {
//   console.log("Future:", queue.future())
// })

// effect(() => {
//   console.log("Now:", queue.now())
// })
