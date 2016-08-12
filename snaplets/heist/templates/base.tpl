<!DOCTYPE html>
<html>
  <head>
    <title>Serendipitous</title>
    <link rel="stylesheet" type="text/css" href="/static/screen.css"/>
    <link rel="stylesheet" href="/static/styles/tomorrow-night-blue.css">
    <link href="/rss" rel="alternate" type="application/rss+xml" title="Serendipitous Articles" />
    <meta name="viewport" content="width=device-width, initial-scale=1">
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
    <div id="static-header">
      <div class="main">
        <img src="/static/logo.png">
        <a href="/"><span>Serendipitous</span></a>
      </div>
      <div class="sub">
        <span>A public journal by <a href="/pages/about">Justin Thomas</a></span>
      </div>
    </div>
    <div id="content">
      <apply-content/>
    </div>
    <div id="catalog">
      <ul>
        <articles>
          <a href="/articles/${articleReference}"><li><articleTitle/></li></a>
        </articles>
      </ul>
    </div>
  </body>
</html>
