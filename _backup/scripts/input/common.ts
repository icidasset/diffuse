export function isAudioFile(filename: string) {
  return filename.match(/\.(flac|m4a|mp3|mp4|ogg|opus|wav|webm)$/);
}
