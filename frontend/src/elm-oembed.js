customElements.define(
  "oembed-element",
  class extends HTMLElement {
    connectedCallback() {
      let shadow = this.attachShadow({ mode: "closed" });
      const urlAttr = this.getAttribute("url");
      if (urlAttr) {
        renderOembed(shadow, urlAttr, {
          maxwidth: this.getAttribute("maxwidth"),
          maxheight: this.getAttribute("maxheight")
        });
      } else {
        const discoverUrl = this.getAttribute("discover-url");
        if (discoverUrl) {
          getDiscoverUrl(discoverUrl, function (discoveredUrl) {
            if (discoveredUrl) {
              renderOembed(shadow, discoveredUrl, null);
            }
          });
        }
      }
    }
  }
);

/**
 *
 * @param {ShadowRoot} shadow
 * @param {string} urlToEmbed
 * @param {{maxwidth: string?; maxheight: string?}?} options
 */
function renderOembed(shadow, urlToEmbed, options) {
  let apiUrlBuilder = new URL(
    // This API should always deliver JSON
    `https://pareto.space/api/oembed?url=${urlToEmbed}`
    // `http://localhost:4000/api/oembed?url=${urlToEmbed}`
  );
  if (options && options.maxwidth) {
    apiUrlBuilder.searchParams.set("maxwidth", options.maxwidth);
  }
  if (options && options.maxheight) {
    apiUrlBuilder.searchParams.set("maxheight", options.maxheight);
  }
  const apiUrl = apiUrlBuilder.toString();
  httpGetAsync(apiUrl, rawResponse => {
    const response = JSON.parse(rawResponse);

    switch (response.type) {
      case "rich":
        tryRenderingHtml(shadow, response, urlToEmbed);
        break;
      case "video":
        tryRenderingHtml(shadow, response, urlToEmbed);
        break;
      case "photo":
        let img = document.createElement("img");
        img.setAttribute("src", response.url);
        if (options) {
          img.setAttribute(
            "style",
            `max-width: ${options.maxwidth}px; max-height: ${options.maxheight}px;`
          );
        }
        shadow.appendChild(img);
        break;
      default:
        break;
    }
  });
}

/**
 * @param {{
    height: ?number;
    width: ?number;
    html: any;
}} response
 * @param {ShadowRoot} shadow
 */
function tryRenderingHtml(shadow, response, urlToEmbed) {
  if (response && typeof response.html) {
    let iframe = createIframe(response, urlToEmbed);
    shadow.appendChild(iframe);
    let refetchedIframe = shadow.querySelector("iframe");

    function checkRendered() {
      const iframeBody = refetchedIframe.contentWindow.document.body;
      if (iframeBody) {
        const tweet = iframeBody.querySelector('.twitter-tweet');
        if (tweet) {
          const isRendered = tweet.offsetHeight > 0 && tweet.offsetWidth > 0;
          if (isRendered) {
            fixSize(refetchedIframe, iframe, response)
            observer.disconnect(); // Stop observing after rendering
          }
        }
      }
    }

    const observer = new MutationObserver(() => {
      checkRendered();
    });

    iframe.onload = function () {
      if (response.provider_name === "Twitter") {
        // wait for tweet to be rendered
        const iframeBody = iframe.contentWindow.document.body;
        observer.observe(iframeBody, {
          childList: true,
          subtree: true,
        });
      } else {
        fixSize(refetchedIframe, iframe, response)
      }
    };

    function fixSize(refetchedIframe, iframe, response) {
      var width = null;
      if (response.width) {
        if (typeof response.width === "number") {
          width = response.width;
        } else {
          if (response.width.endsWith("%")) {
            width = response.width;
          } else {
            width = parseInt(response.width);
          }
        }
      } else {
        width = iframe.contentWindow.document.body.scrollWidth;
      }
      var height = null;
      if (response.height) {
        if (typeof response.height === "number") {
          height = response.height;
        } else {
          if (response.height.endsWith("%")) {
            height = response.height;
          } else {
            height = parseInt(response.height);
          }
        }
      } else {
        height = iframe.contentWindow.document.body.scrollHeight;
      }
      if (refetchedIframe) {
        refetchedIframe.setAttribute(
          "height",
          // @ts-ignore
          (height + 20).toString()
        );

        refetchedIframe.setAttribute(
          "width",
          // @ts-ignore
          (width + 20).toString()
        );
      }
    }
  }
}

/**
 * @param {{ height: number?; width: number?; html: string; }} response
 * @returns {HTMLIFrameElement}
 */
function createIframe(response, urlToEmbed) {
  let iframe = document.createElement("iframe");
  iframe.setAttribute("border", "0");
  iframe.setAttribute("frameborder", "0");
  iframe.setAttribute("height", ((response.height || 500) + 20).toString());
  iframe.setAttribute("width", ((response.width || 500) + 20).toString());
  iframe.setAttribute("style", "max-width: 100%;");
  iframe.srcdoc = processIframeSrc(response, urlToEmbed);
  return iframe;
}

function processIframeSrc(response, urlToEmbed) {
  if (response['provider-name'] === 'SoundCloud') {
    // take color parameter for embedded player from original url to iframe src URL
    const oembedUrl = new URL(urlToEmbed);
    const urlParam = oembedUrl.searchParams.get('url');
    const soundCloudUrl = new URL(urlParam);
    const colorParam = soundCloudUrl.searchParams.get('color');
    if (colorParam) {
      const iframeSrc = response.html.match(/src="([^"]+)"/)[1];
      const updatedSrc = `${iframeSrc}&color=${colorParam}`;
      return response.html.replace(iframeSrc, updatedSrc);
    }
  }
  return response.html;
}

/**
 * @param {string} url
 * @param {{ (discoveredUrl: string?): void;}} callback
 */
function getDiscoverUrl(url, callback) {
  let apiUrl = new URL(`https://cors-anywhere.herokuapp.com/${url}`).toString();
  httpGetAsync(apiUrl, function (response) {
    let dom = document.createElement("html");
    dom.innerHTML = response;
    /** @type {HTMLLinkElement | null} */ const oembedTag = dom.querySelector(
      'link[type="application/json+oembed"]'
    );
    callback(oembedTag && oembedTag.href);
  });
}

/**
 * @param {string} theUrl
 * @param {{ (rawResponse: string): void }} callback
 */
function httpGetAsync(theUrl, callback) {
  var xmlHttp = new XMLHttpRequest();
  xmlHttp.onreadystatechange = function () {
    if (xmlHttp.readyState == 4 && xmlHttp.status == 200)
      callback(xmlHttp.responseText);
  };
  xmlHttp.open("GET", theUrl, true); // true for asynchronous
  xmlHttp.send(null);
}
