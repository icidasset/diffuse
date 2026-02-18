const prefix = "diffuse/output/raw/atproto/oauth";
const redirect_path = sessionStorage.getItem(`${prefix}/redirect_path`) ?? "/";

sessionStorage.removeItem(`${prefix}/redirect_path`);
location.assign(`${redirect_path}${location.hash}`);
