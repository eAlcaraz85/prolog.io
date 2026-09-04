/**
 * Campo "Buscar en el índice" en la barra lateral (estilo Read the Docs).
 * Filtra entradas del TOC por texto; requiere jQuery (ya cargado por el tema).
 */
(function ($) {
  "use strict";

  function filterToc($root, query) {
    var q = (query || "").trim().toLowerCase();
    var $lis = $root.find("li");
    if (!q) {
      $lis.show();
      return;
    }
    var ordered = $lis.get().sort(function (a, b) {
      return $(b).parents("li").length - $(a).parents("li").length;
    });
    $.each(ordered, function (_, el) {
      var $li = $(el);
      var linkText = $li.children("a").first().text().toLowerCase();
      var childVisible =
        $li.children("ul").children("li").filter(":visible").length > 0;
      var match = linkText.indexOf(q) !== -1;
      $li.toggle(match || childVisible);
    });
  }

  $(document).ready(function () {
    var $tocBlock = $("#text-table-of-contents");
    if (!$tocBlock.length) {
      return;
    }

    var $h2 = $("#table-of-contents > h2").first();
    if (!$h2.length) {
      return;
    }

    /* Un solo bloque visual: título "Índice" + buscador (como RTD) */
    var $indexHead = $(
      '<div class="ms-toc-index-head" role="navigation" aria-label="Índice del documento"></div>'
    );
    $h2.wrap($indexHead);
    $indexHead = $h2.parent();

    var $wrap = $('<div class="ms-toc-search-wrap"></div>');
    $wrap.append(
      '<label class="ms-visually-hidden" for="ms-toc-search">Buscar en el índice</label>'
    );
    var $input = $(
      '<input type="search" id="ms-toc-search" placeholder="Buscar en el índice…" autocomplete="off" spellcheck="false" />'
    );
    $wrap.append($input);
    $indexHead.append($wrap);

    function syncSidebarPadding() {
      var $postamble = $("#postamble");
      var $tableOfContents = $("#table-of-contents");
      var extra = $wrap.outerHeight() || 0;
      var base = $postamble.outerHeight() || 0;
      $tableOfContents.css({ paddingBottom: base + extra });
    }

    syncSidebarPadding();
    $(window).on("load", syncSidebarPadding);

    var debounce;
    $input.on("input", function () {
      clearTimeout(debounce);
      var val = $(this).val();
      debounce = setTimeout(function () {
        filterToc($tocBlock, val);
      }, 120);
    });
  });
})(jQuery);
