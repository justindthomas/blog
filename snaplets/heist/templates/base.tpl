<!DOCTYPE html>
<html>
  <head>
    <title>Serendipitous</title>
    <link rel="stylesheet" type="text/css" href="/sass/screen.css"/>
    <link rel="stylesheet" href="/static/styles/tomorrow-night-blue.css">
    <link href="/rss" rel="alternate" type="application/rss+xml" title="Serendipitous Articles" />
    <!-- <meta name="viewport" content="width=device-width, initial-scale=1"> -->
    <script src="/static/highlight.pack.js"></script>
    <script>hljs.initHighlightingOnLoad();</script>
    <script src="/static/jquery-3.1.0.min.js"></script>
    <script src="/static/application.js"></script>
  </head>
  <body ontouchstart="">
    <header>
      <p>
        <img src="/static/logo.png">
        <a href="/">Serendipitous</a>
      </p>
      <p><a href="/pages/about">About</a></p>
    </header>
    <div id="banner">
      <div class="logo">
        <img src="/static/logo.png">
        <div class="title">
          <a href="/"><span>Serendipitous</span></a>
          <span>A public journal by <a href="/pages/about">Justin Thomas</a></span>
        </div>
      </div>
    </div>
    <div id="catalog">
      <h3>Other Articles</h3>
      <ul>
        <articles>
          <a href="/articles/${articleReference}">
            <li>
              <p><articleTitle/></p>
              <p><articleCreation/></p>
            </li>
          </a>
        </articles>
      </ul>
    </div>
    <div id="content">
      <apply-content/>
    </div>
  </body>
</html>
