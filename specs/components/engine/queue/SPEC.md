# Queue engine

This is a simple queue component to queue up audio to play in the future, keep track of what is playing now, and what has played in the past.


## Requirements

- Keep track of what to play in the future, what is playing now (if anything), and what has played in the past. Keep this state in signals so UIs can reflect state changes.
- Must be able to differentiate between "manual" and "automatic" items. The former is added by the user manually through a UI action using, for example, the `add` method. The latter is an automatic trigger, eg. where the queue is filled up automatically for infinite playback.
- The queue can be "filled" with items that were "supplied" earlier, may be shuffled.
- Items have a `key` property which can be used to remove (`expel`) it or `move` it.
- `shift` method takes the first item from the future list and sets it as "now playing". If something was already in the "now playing" state, move that to past list as the last item.
- `unshift` is the inverse of `shift`.
- Must be able to add items in front of the queue, as well as the end of the queue (beginning and ending of the future list respectively).
- Should also be able to `clear` items from the queue, be it automatic items, manual items, or both at the same time.
