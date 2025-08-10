import type { Track } from "@applets/core/types";

export type Item<Stats = TrackStats, Tags = TrackTags> = Track & {
  manualEntry?: boolean;
};

export interface State<Stats = TrackStats, Tags = TrackTags> {
  past: Item<Stats, Tags>[];
  now: Item<Stats, Tags> | null;
  future: Item<Stats, Tags>[];
}
