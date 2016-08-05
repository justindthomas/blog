<apply template="base">
    <latest>
      <div>
        <div class="title"><p><articleTitle/></p></div>
        <div class="creation"><p><a href="/articles/${articleReference}"><articleCreation/></a></p></div>
        <div class="article"><articleContent/></div>
        <ul id="navigation" class="halvsies">
          <a href="/articles/${articlePrev}" id="previous">
            <li class="button">
              <div>Previous</div>
            </li>
          </a>
          <a href="#" class="not-active" id="next">
            <li class="button">
              <div>Next</div>
            </li>
          </a>
        </ul>
      </div>
    </latest>
</apply>
