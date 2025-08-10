import { applet, reactive } from "@scripts/applet/common";

////////////////////////////////////////////
// 🗂️ Applets
////////////////////////////////////////////
import type * as AudioEngine from "@applets/engine/audio/types.d.ts";
import type * as QueueEngine from "@applets/engine/queue/types.d.ts";

import type * as AudioUI from "@applets/theme/pilot/audio/types";

const engine = {
  audio: await applet<AudioEngine.State>("/engine/audio"),
  queue: await applet<QueueEngine.State>("/engine/queue"),
};

const orchestrator = {
  queueAudio: applet("/orchestrator/queue-audio"),
  queueTracks: applet("/orchestrator/queue-tracks"),
  processTracks: applet("/orchestrator/process-tracks"),
};

const ui = {
  audio: await applet<AudioUI.State>("/theme/pilot/audio/", { setHeight: true }),
};

////////////////////////////////////////////
// ⚙️ [Connections → Engines]
// 🔉 AUDIO
////////////////////////////////////////////

// NOTE:
// These could probably be optimised, but it works.

reactive(
  engine.audio,
  (data) =>
    data.isPlaying && (data.items[engine.queue.data.now?.id ?? Infinity]?.isPlaying ?? false),
  (isPlaying) => ui.audio.sendAction("modifyIsPlaying", isPlaying),
);

reactive(
  engine.audio,
  (data) => data.items[engine.queue.data.now?.id ?? Infinity]?.progress ?? 0,
  (progress: number) => ui.audio.sendAction("modifyProgress", progress),
);

////////////////////////////////////////////
// 🌅 [Connections → UI]
// 🔉 AUDIO
////////////////////////////////////////////

let initialAudioChecked = false;

reactive(
  ui.audio,
  (data) => data.isPlaying,
  async (isPlaying) => {
    const audioId = engine.queue.data.now?.id;

    // Sync audio state and ui state
    // TODO: Figure out a better way to do this
    if (!initialAudioChecked) {
      if (engine.audio.data.isPlaying && !isPlaying) {
        ui.audio.sendAction("modifyIsPlaying", true);
        initialAudioChecked = true;
        return;
      }
    }

    // Otherwise just control the audio
    if (isPlaying) {
      engine.audio.sendAction("play", { audioId });
    } else {
      engine.audio.sendAction("pause", { audioId });
    }
  },
);

reactive(
  ui.audio,
  (data: AudioUI.State) => data.seekPosition,
  (seekPosition) => {
    if (seekPosition !== undefined && engine.queue.data.now?.id) {
      engine.audio.sendAction("seek", {
        percentage: seekPosition,
        audioId: engine.queue.data.now.id,
      });
    }
  },
);
