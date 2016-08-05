<apply template="base">
  <article>
    <div>
      <div class="title"><p><articleTitle/></p></div>
      <div class="creation"><p><articleCreation/></p></div>
      <div class="article"><articleContent/></div>
      <ul id="navigation" class="halvsies">
        <a href="/articles/${articlePrev}" id="previous">
          <li class="button">
            <div>Previous</div>
          </li>
        </a>
        <a href="/articles/${articleNext}" id="next">
          <li class="button">
            <div>Next</div>
          </li>
        </a>
      </ul>
    </div>
  </article>
</apply>
