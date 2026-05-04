const prefix = "oauth/callback";
const redirect_path = localStorage.getItem(`${prefix}/redirect_path`) ?? "/";

localStorage.removeItem(`${prefix}/redirect_path`);
location.assign(`${redirect_path}${location.hash}`);
