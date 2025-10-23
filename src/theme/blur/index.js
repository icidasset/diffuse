import * as Audio from "@component/engine/audio/element.js";
import * as Queue from "@component/engine/queue/element.js";
import * as Metadata from "@component/processor/metadata/element.js";

import { component } from "@common/element.js";
import { effect, signal, untracked } from "@common/signal.js";

/**
 * @import {Item} from "@component/engine/queue/types.d.ts"
 */

const audio = component(Audio);
const queue = component(Queue);
const metadata = component(Metadata);

// METADATA

// const resp = await metadata.supply({
//   urls: { get: url, head: url },
// });

// console.log(resp);

// QUEUE

effect(() => {
  const now = queue.now();
  console.log("NOW", now);

  if (now === null) return;

  untracked(() => {
    audio.supply({
      audio: [
        {
          id: now.id,
          isPreload: false,
          url: now.uri,
        },
      ],
    });
  });
});

queue.pool([
  {
    id: "test",
    uri:
      "https://archive.org/download/deathofsalesmans00mill/01_Side_1_Death_of_a_salesman_-_Introduction_Act_1__Part_1.mp3",
  },
]);
