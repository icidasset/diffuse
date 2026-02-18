const url = new URL(document.location.href);
const redirect_path = url.searchParams.get("redirect_path") ?? "";

url.searchParams.delete("redirect_path");

location.assign(
  `${redirect_path}?${url.searchParams.toString()}${url.hash}`,
);
