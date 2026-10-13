/* estudo.js — comportamento comum a todas as páginas do site de estudo.
   Capítulos: trilha, trilho lateral, atividades, referências, retomada, revisão.
   Páginas gerais (início, revisão para prova, fórmulas): usam só tipografar e ligarQuiz.
   window.SITE (assets/site.js, gerado pelo montar_site.py) é opcional: sem ele, a página funciona sozinha. */

function tipografar(el) {
  if (window.MathJax && MathJax.typesetPromise) return MathJax.typesetPromise([el]).catch(function () {});
  return Promise.resolve();
}

/* ===== Múltipla escolha ===== */
function ligarQuiz(q, aoResponder) {
  var certa = q.dataset.correta, primeira = true;
  q.querySelectorAll('.alts button').forEach(function (b) {
    b.addEventListener('click', function () {
      var op = b.dataset.op;
      q.querySelectorAll('.alts button').forEach(function (x) { x.classList.remove('certa', 'errada'); });
      q.querySelectorAll('.fb').forEach(function (f) { f.classList.remove('mostrar', 'ok', 'no'); });
      b.classList.add(op === certa ? 'certa' : 'errada');
      var fb = q.querySelector('.fb[data-op="' + op + '"]');
      if (fb) fb.classList.add('mostrar', op === certa ? 'ok' : 'no');
      if (aoResponder) aoResponder(op === certa, primeira);
      primeira = false;
    });
  });
}

(function () {
  if (!document.getElementById('trilha')) return;
  var passos = Array.prototype.slice.call(document.querySelectorAll('#trilha .passo'));
  passos.forEach(function (p) { if (p.querySelector(':scope > .apoio')) p.classList.add('com-apoio'); });
  var trilha = document.getElementById('trilha'), resumo = document.getElementById('resumo'), painel = document.getElementById('painel');

  /* ===== Estado salvo no navegador ===== */
  /* A chave vem de <meta name="estudo-chave" content="livro/cap-08">; a página inicial do site lê a mesma chave. */
  var metaChave = document.querySelector('meta[name="estudo-chave"]');
  var idPagina = metaChave ? metaChave.content : document.title;
  var chave = 'estatistica-tdah:' + idPagina;
  var estado = { atual: 0, feitos: {}, prev: {}, ultimo: 0 };
  try {
    var salvo = JSON.parse(localStorage.getItem(chave) || 'null');
    if (salvo && typeof salvo.atual === 'number') {
      estado.atual = salvo.atual; estado.feitos = salvo.feitos || {}; estado.prev = salvo.prev || {}; estado.ultimo = salvo.ultimo || 0;
    }
  } catch (e) {}
  if (estado.atual >= passos.length) estado.atual = 0;
  /* Links vindos da revisão ou das fórmulas: capitulo.html#passo-7 abre direto no passo 7. */
  var hashPasso = /^#passo-(\d+)$/.exec(location.hash);
  if (hashPasso && +hashPasso[1] >= 1 && +hashPasso[1] <= passos.length) estado.atual = +hashPasso[1] - 1;
  var voltouDepois = estado.ultimo > 0 && Date.now() - estado.ultimo > 30 * 60 * 1000;
  function salvar() {
    estado.ultimo = Date.now();
    estado.onde = nomeSecao(secaoDe(estado.atual)) + ' — ' + (passos[estado.atual].dataset.titulo || '');
    estado.total = passos.length;
    try { localStorage.setItem(chave, JSON.stringify(estado)); } catch (e) {}
  }

  /* ===== Seções do livro e passos ===== */
  var secoes = Array.prototype.slice.call(trilha.querySelectorAll('.secao'));
  if (!secoes.length) secoes = [trilha];
  var tituloPagina = (document.querySelector('h1') || {}).textContent || '';
  function secaoDe(i) { var s = passos[i].closest('.secao'); return s ? secoes.indexOf(s) : 0; }
  function passosDa(k) { return passos.map(function (p, i) { return i; }).filter(function (i) { return secaoDe(i) === k; }); }
  function nomeSecao(k) {
    var s = secoes[k];
    if (s === trilha) return tituloPagina;
    return (s.dataset.numero ? s.dataset.numero + ' ' : '') + (s.dataset.titulo || '');
  }
  /* ===== Passos da seção atual, no topo ===== */
  var subtopicos = document.getElementById('subtopicos');
  var sublinhado = document.createElement('div'); sublinhado.className = 'sub-sublinhado';
  function montarSubtopicos() {
    var ks = secaoDe(estado.atual), dela = passosDa(ks), alvo = null;
    subtopicos.innerHTML = '';
    var nome = document.createElement('span'); nome.className = 'sub-secao'; nome.textContent = nomeSecao(ks);
    subtopicos.appendChild(nome);
    if (dela.length > 1) {
      dela.forEach(function (i) {
        var b = document.createElement('button'); b.type = 'button';
        b.textContent = passos[i].dataset.titulo || 'Passo';
        if (i === estado.atual) { b.classList.add('atual'); b.setAttribute('aria-current', 'step'); alvo = b; }
        b.addEventListener('click', function () { ir(i); });
        subtopicos.appendChild(b);
      });
    }
    /* data-livro (onde o assunto está no livro) fica só no código: o aluno estuda pelo site, não pelo livro */
    subtopicos.appendChild(sublinhado);
    /* O sublinhado desliza até o passo atual (ler offsetLeft antes força o estilo antigo, e a transição acontece). */
    if (alvo) {
      var x = alvo.offsetLeft, y = alvo.offsetTop + alvo.offsetHeight - 1, w = alvo.offsetWidth;
      sublinhado.style.opacity = '1';
      sublinhado.style.transform = 'translate(' + x + 'px, ' + y + 'px)';
      sublinhado.style.width = w + 'px';
    } else sublinhado.style.opacity = '0';
  }

  /* ===== Trilho lateral ===== */
  var trilho = document.getElementById('trilho');
  var tracos = document.getElementById('trilho-tracos');
  var listaTrilho = document.getElementById('trilho-lista');
  var marcador = document.createElement('div'); marcador.className = 'trilho-marcador';
  document.getElementById('trilho-cap').textContent = tituloPagina;
  secoes.forEach(function () { var t = document.createElement('div'); t.className = 'traco'; tracos.appendChild(t); });
  var hoverFino = window.matchMedia && window.matchMedia('(hover: hover) and (pointer: fine)').matches;
  var tAbrir = null, tFechar = null;
  function abrirTrilho() { clearTimeout(tFechar); trilho.classList.add('aberto'); trilho.setAttribute('aria-expanded', 'true'); }
  function fecharTrilho() { clearTimeout(tAbrir); trilho.classList.remove('aberto'); trilho.setAttribute('aria-expanded', 'false'); }
  if (hoverFino) {
    trilho.addEventListener('mouseenter', function () { clearTimeout(tFechar); tAbrir = setTimeout(abrirTrilho, 70); });
    trilho.addEventListener('mouseleave', function () { clearTimeout(tAbrir); tFechar = setTimeout(fecharTrilho, 260); });
  }
  trilho.addEventListener('click', function (e) { e.stopPropagation(); if (!trilho.classList.contains('aberto')) abrirTrilho(); });
  trilho.addEventListener('focusin', abrirTrilho);
  trilho.addEventListener('focusout', function (e) { if (!trilho.contains(e.relatedTarget)) fecharTrilho(); });
  document.addEventListener('click', fecharTrilho);
  document.addEventListener('keydown', function (e) { if (e.key === 'Escape') fecharTrilho(); });
  document.getElementById('btn-ir-resumo').addEventListener('click', function () { fecharTrilho(); abrirAba('resumo'); });
  function irPeloTrilho(i) { fecharTrilho(); if (trilha.hidden) abrirAba('trilha'); ir(i); }

  function montarTrilho() {
    var ks = secaoDe(estado.atual), n = 0, alvo = null;
    listaTrilho.innerHTML = ''; listaTrilho.appendChild(marcador);
    secoes.forEach(function (s, k) {
      var dela = passosDa(k);
      var b = document.createElement('button'); b.type = 'button'; b.className = 'trilho-secao';
      b.style.setProperty('--i', n++);
      b.textContent = nomeSecao(k);
      if (k === ks) b.classList.add('atual');
      else if (dela.every(function (i) { return estado.feitos[i]; })) b.classList.add('vista');
      b.addEventListener('click', function () { irPeloTrilho(dela[0]); });
      listaTrilho.appendChild(b);
      if (k === ks && dela.length > 1) {
        dela.forEach(function (i, j) {
          var bp = document.createElement('button'); bp.type = 'button'; bp.className = 'trilho-passo';
          bp.style.setProperty('--i', n++);
          bp.textContent = (j + 1) + '. ' + (passos[i].dataset.titulo || 'Passo');
          if (i === estado.atual) { bp.classList.add('atual'); alvo = bp; }
          bp.addEventListener('click', function () { irPeloTrilho(i); });
          listaTrilho.appendChild(bp);
        });
      }
    });
    document.getElementById('trilho-rodape').style.setProperty('--i', n);
    Array.prototype.forEach.call(tracos.children, function (t, k) {
      t.classList.toggle('atual', k === ks);
      t.classList.toggle('vista', k !== ks && passosDa(k).every(function (i) { return estado.feitos[i]; }));
    });
    /* O marcador escorrega até o passo atual (ler offsetTop antes força o estilo antigo, e a transição acontece). */
    if (alvo) {
      var y = alvo.offsetTop, h = alvo.offsetHeight;
      marcador.style.opacity = '1';
      marcador.style.transform = 'translateY(' + y + 'px)';
      marcador.style.height = h + 'px';
    } else marcador.style.opacity = '0';
    /* Sem rolagem interna: se o capítulo for longo demais para a tela, o trilho fica compacto. */
    trilho.classList.remove('compacto');
    if (trilho.querySelector('.trilho-painel').scrollHeight > window.innerHeight) trilho.classList.add('compacto');
  }

  function ir(i) {
    estado.feitos[estado.atual] = true;
    estado.atual = Math.max(0, Math.min(passos.length - 1, i));
    render(); salvar();
    /* Rola até o topo da trilha, não da página: o título fica para trás e sobra mais tela para o passo. */
    window.scrollTo({ top: Math.max(0, trilha.getBoundingClientRect().top + window.scrollY - 8), behavior: 'smooth' });
  }
  function render() {
    passos.forEach(function (p, i) { p.hidden = i !== estado.atual; });
    document.getElementById('anterior').style.visibility = estado.atual === 0 ? 'hidden' : 'visible';
    document.getElementById('proximo').textContent = estado.atual === passos.length - 1 ? 'Ver resumo' : 'Próximo';
    montarTrilho();
    montarSubtopicos();
    ajustarApoios();
  }
  document.getElementById('anterior').addEventListener('click', function () { ir(estado.atual - 1); });
  document.getElementById('proximo').addEventListener('click', function () {
    if (estado.atual === passos.length - 1) { estado.feitos[estado.atual] = true; salvar(); abrirAba('resumo'); render(); }
    else ir(estado.atual + 1);
  });
  document.addEventListener('keydown', function (e) {
    if (trilha.hidden) return;
    if (e.target && /INPUT|TEXTAREA|SELECT/.test(e.target.tagName)) return;
    if (e.key === 'ArrowRight') ir(estado.atual + 1);
    if (e.key === 'ArrowLeft') ir(estado.atual - 1);
  });

  function abrirAba(nome) {
    document.querySelectorAll('.abas button').forEach(function (b) { b.setAttribute('aria-selected', b.dataset.aba === nome ? 'true' : 'false'); });
    trilha.hidden = nome !== 'trilha';
    resumo.hidden = nome !== 'resumo';
    painel.hidden = true;
    window.scrollTo({ top: 0 });
    if (nome === 'trilha') { ajustarApoios(); montarSubtopicos(); }
  }
  document.querySelectorAll('.abas button').forEach(function (b) { b.addEventListener('click', function () { abrirAba(b.dataset.aba); }); });

  /* ===== Coluna de apoio sem rolagem interna =====
     Cabe na tela: prende no alto. Não cabe: rola com a página e prende pela base (topo negativo). */
  var telaLarga = window.matchMedia('(min-width: 960px)');
  function ajustarApoios() {
    passos.forEach(function (p) {
      var a = p.querySelector(':scope > .apoio');
      if (!a || p.hidden) return;
      if (!telaLarga.matches) { a.style.top = ''; return; }
      var margem = 16, h = a.offsetHeight, vh = window.innerHeight;
      a.style.top = (h + 2 * margem <= vh ? margem : vh - h - margem) + 'px';
    });
  }
  window.addEventListener('resize', ajustarApoios);
  if (window.ResizeObserver) {
    var ro = new ResizeObserver(ajustarApoios);
    document.querySelectorAll('.passo > .apoio').forEach(function (a) { ro.observe(a); });
  }

  /* ===== Pare e tente ===== */
  function ligarRevelar(raiz) {
    raiz.querySelectorAll('[data-revelar]').forEach(function (b) {
      b.addEventListener('click', function () {
        var r = b.parentNode.querySelector('.resposta');
        r.hidden = !r.hidden;
        b.textContent = r.hidden ? 'Ver resposta' : 'Esconder resposta';
      });
    });
  }
  ligarRevelar(trilha);

  trilha.querySelectorAll('.quiz').forEach(function (q) { ligarQuiz(q); });

  /* ===== Prever → observar → explicar ===== */
  trilha.querySelectorAll('.prever').forEach(function (pv, k) {
    var sim = pv.dataset.sim ? document.getElementById(pv.dataset.sim) : null;
    var id = pv.dataset.sim || ('prever-' + k);
    var botoes = pv.querySelectorAll('.alts button');
    var explica = pv.querySelector('.explica');
    var aviso = document.createElement('p'); aviso.className = 'aviso-prever'; aviso.setAttribute('aria-live', 'polite');
    var btnVer = document.createElement('button'); btnVer.className = 'btn pequeno'; btnVer.textContent = 'Ver explicação'; btnVer.hidden = true;
    pv.insertBefore(aviso, explica); pv.insertBefore(btnVer, explica);
    function travar(t) { if (!sim) return; sim.classList.toggle('travada', t); sim.inert = t; }
    function escolher(op, restaurando) {
      botoes.forEach(function (b) { b.classList.toggle('escolhida', b.dataset.op === op); });
      estado.prev[id] = op; salvar();
      travar(false);
      aviso.textContent = sim ? 'Previsão registrada. Agora mexa na simulação e confira.' : 'Previsão registrada.';
      if (!sim || restaurando) btnVer.hidden = false;
    }
    botoes.forEach(function (b) { b.addEventListener('click', function () { if (explica.hidden) escolher(b.dataset.op); }); });
    if (sim) {
      travar(true);
      aviso.textContent = 'A simulação libera depois da sua previsão.';
      ['input', 'click'].forEach(function (ev) {
        sim.addEventListener(ev, function () { if (!sim.classList.contains('travada') && explica.hidden) btnVer.hidden = false; });
      });
    }
    btnVer.addEventListener('click', function () {
      var op = estado.prev[id], acertou = false;
      botoes.forEach(function (b) {
        if (b.hasAttribute('data-certa')) { b.classList.add('certa'); if (b.dataset.op === op) acertou = true; }
        else if (b.dataset.op === op) b.classList.add('errada');
      });
      aviso.textContent = acertou ? 'Sua previsão bateu.' : 'Sua previsão não bateu. Veja por quê:';
      explica.hidden = false; btnVer.hidden = true;
    });
    if (estado.prev[id]) escolher(estado.prev[id], true);
  });

  /* ===== Exemplo com lacunas ===== */
  function numero(s) {
    s = String(s).trim().replace(/\s/g, '').replace('−', '-');
    if (s.indexOf(',') >= 0) s = s.replace(/\./g, '').replace(',', '.');
    return s === '' ? NaN : Number(s);
  }
  trilha.querySelectorAll('.lacunas').forEach(function (bx) {
    var campos = bx.querySelectorAll('input.lacuna');
    var btn = bx.querySelector('[data-conferir]');
    var res = document.createElement('p'); res.className = 'res-lacunas'; res.setAttribute('aria-live', 'polite');
    btn.insertAdjacentElement('afterend', res);
    var resolucao = bx.querySelector('.resolucao'), btnRes = null;
    if (resolucao) {
      btnRes = document.createElement('button'); btnRes.className = 'btn pequeno'; btnRes.textContent = 'Ver resolução'; btnRes.hidden = true;
      btnRes.style.marginTop = '.5rem';
      resolucao.parentNode.insertBefore(btnRes, resolucao);
      btnRes.addEventListener('click', function () {
        resolucao.hidden = !resolucao.hidden;
        btnRes.textContent = resolucao.hidden ? 'Ver resolução' : 'Esconder resolução';
      });
    }
    function conferir() {
      var certas = 0, vazias = 0;
      campos.forEach(function (c) {
        var v = numero(c.value), r = numero(c.dataset.resp);
        var tol = c.dataset.tol !== undefined ? numero(c.dataset.tol) : Math.abs(r) * 0.005;
        c.classList.remove('ok', 'no');
        if (isNaN(v)) { vazias++; return; }
        var ok = Math.abs(v - r) <= tol + 1e-9;
        c.classList.add(ok ? 'ok' : 'no');
        if (ok) certas++;
      });
      var tudo = certas === campos.length;
      res.className = 'res-lacunas ' + (tudo ? 'ok' : 'no');
      res.textContent = tudo ? 'Tudo certo.' : certas + ' de ' + campos.length + ' certas' + (vazias ? ' (' + vazias + ' em branco)' : '') + '. Corrija as marcadas.';
      var dica = bx.querySelector('.dica');
      if (dica && !tudo) dica.hidden = false;
      if (btnRes) btnRes.hidden = false;
    }
    btn.addEventListener('click', conferir);
    campos.forEach(function (c) {
      c.setAttribute('inputmode', 'decimal'); c.setAttribute('autocomplete', 'off');
      c.addEventListener('keydown', function (e) { if (e.key === 'Enter') conferir(); });
      c.addEventListener('input', function () { c.classList.remove('ok', 'no'); });
    });
  });

  /* ===== Ache o erro ===== */
  trilha.querySelectorAll('.acheerro').forEach(function (ae) {
    var fb = document.createElement('div'); fb.className = 'fb'; fb.setAttribute('aria-live', 'polite');
    ae.appendChild(fb);
    ae.querySelectorAll('.linhas > li').forEach(function (li) {
      var b = document.createElement('button'); b.type = 'button';
      while (li.firstChild) b.appendChild(li.firstChild);
      li.appendChild(b);
      b.addEventListener('click', function () {
        var errada = li.hasAttribute('data-errada');
        b.classList.add(errada ? 'achou' : 'correta');
        fb.textContent = (errada ? 'Achou. ' : '') + (li.dataset.fb || (errada ? '' : 'Esta linha está certa.'));
        fb.className = 'fb mostrar ' + (errada ? 'ok' : 'no');
        tipografar(fb);
      });
    });
  });

  /* ===== Referências: balão (fórmulas, definições), gaveta e painel ===== */
  var balao = document.getElementById('balao');
  var gaveta = document.getElementById('gaveta');
  var fixado = false, refAtual = null;
  var temHover = window.matchMedia && window.matchMedia('(hover: hover) and (pointer: fine)').matches;
  var nomesTipo = { formula: 'Fórmula', definicao: 'Definição', exemplo: 'Exemplo' };

  /* Âncora deste capítulo; se não houver, a de um capítulo anterior (assets/refs.js, gerado pelo montar). */
  function ancora(id) {
    var el = document.querySelector('[data-ref-id="' + id + '"]');
    var r = window.REFS && window.REFS[id];
    if (el || !r) return el;
    var caixa = document.createElement('div'); caixa.innerHTML = r.html;
    el = caixa.firstElementChild;
    if (!el) return null;
    /* figura desenhada pelo script do outro capítulo chega vazia: some (o link abre o original) */
    el.querySelectorAll('svg').forEach(function (s) { if (!s.children.length) s.remove(); });
    el.dataset.refTipo = r.tipo; el.dataset.refNome = r.nome || '';
    el.dataset.refCap = r.numero; el.dataset.refPasso = r.passo; el.dataset.refArquivo = r.arquivo;
    return el;
  }
  function passoDe(el) { var s = el.closest('.passo'); return s ? passos.indexOf(s) : -1; }
  function copia(el) {
    var c = el.cloneNode(true);
    c.removeAttribute('data-ref-id');
    c.querySelectorAll('[id]').forEach(function (x) { x.removeAttribute('id'); });
    c.querySelectorAll('.ref').forEach(function (x) { x.outerHTML = x.innerHTML; });
    return c;
  }
  function bloco(el) {
    var d = document.createElement('div');
    var externo = !!el.dataset.refCap, i = externo ? -1 : passoDe(el);
    var r = document.createElement('p'); r.className = 'rotulo-ref';
    var onde = externo ? ' do cap. ' + el.dataset.refCap + ', passo ' + el.dataset.refPasso : (i >= 0 ? ' do passo ' + (i + 1) : '');
    r.textContent = (nomesTipo[el.dataset.refTipo] || 'Referência') + onde + (el.dataset.refNome ? ': ' + el.dataset.refNome : '');
    d.appendChild(r); d.appendChild(copia(el));
    if (externo) {
      /* outro capítulo abre em nova aba: o leitor não perde o lugar onde está */
      var a = document.createElement('a'); a.className = 'ir-passo';
      a.href = '../' + el.dataset.refArquivo + '#passo-' + el.dataset.refPasso;
      a.target = '_blank'; a.rel = 'noopener';
      a.textContent = 'Abrir o cap. ' + el.dataset.refCap + ' nesse passo (nova aba)';
      d.appendChild(a);
    } else if (i >= 0) {
      var b = document.createElement('button'); b.className = 'ir-passo'; b.textContent = 'Ir para o passo ' + (i + 1);
      b.addEventListener('click', function (e) { e.stopPropagation(); irParaAncora(el); });
      d.appendChild(b);
    }
    return d;
  }
  function irParaAncora(el) {
    fecharBalao(); fecharGaveta();
    abrirAba('trilha');
    var i = passoDe(el);
    if (i >= 0 && i !== estado.atual) ir(i);
    setTimeout(function () {
      el.scrollIntoView({ behavior: 'smooth', block: 'center' });
      el.classList.remove('destaque'); void el.offsetWidth; el.classList.add('destaque');
    }, 350);
  }

  function abrirBalao(ref, el) {
    balao.innerHTML = ''; balao.appendChild(bloco(el)); balao.hidden = false; refAtual = ref;
    posicionar();
    tipografar(balao).then(function () { if (refAtual === ref) posicionar(); });
    function posicionar() {
      var r = ref.getBoundingClientRect(), largura = balao.offsetWidth, altura = balao.offsetHeight;
      var esquerda = Math.min(Math.max(12, r.left), window.innerWidth - largura - 12);
      var topo = (r.bottom + altura + 12 > window.innerHeight && r.top > altura + 12) ? r.top - altura - 8 : r.bottom + 8;
      balao.style.left = (esquerda + window.scrollX) + 'px';
      balao.style.top = (topo + window.scrollY) + 'px';
    }
  }
  function fecharBalao() { balao.hidden = true; fixado = false; refAtual = null; }

  /* A gaveta só abre se couber na tela; senão, o conteúdo vai para o painel (página inteira, rolagem normal). */
  var vistaAntes = 'trilha', rolagemAntes = 0;
  function preencher(corpo, elementos, nota) {
    corpo.innerHTML = '';
    elementos.forEach(function (el) { var it = bloco(el); it.className = 'gaveta-item'; corpo.appendChild(it); });
    if (!elementos.length) corpo.textContent = 'Nenhuma fórmula até aqui.';
    if (nota) { var n = document.createElement('p'); n.className = 'nota-painel'; n.textContent = nota; corpo.appendChild(n); }
  }
  function abrirGaveta(titulo, elementos, nota) {
    fecharBalao();
    document.getElementById('gaveta-titulo').textContent = titulo;
    var corpo = document.getElementById('gaveta-corpo');
    preencher(corpo, elementos, nota);
    gaveta.style.visibility = 'hidden'; gaveta.hidden = false;
    tipografar(corpo).then(function () {
      if (gaveta.offsetHeight > window.innerHeight * 0.75) { fecharGaveta(); abrirPainel(titulo, elementos, nota); }
      else gaveta.style.visibility = '';
    });
  }
  function fecharGaveta() { gaveta.hidden = true; gaveta.style.visibility = ''; }
  function abrirPainel(titulo, elementos, nota) {
    if (painel.hidden) { vistaAntes = trilha.hidden ? 'resumo' : 'trilha'; rolagemAntes = window.scrollY; }
    trilha.hidden = true; resumo.hidden = true;
    document.getElementById('painel-titulo').textContent = titulo;
    var corpo = document.getElementById('painel-corpo');
    preencher(corpo, elementos, nota);
    corpo.className = 'painel-lista';
    painel.hidden = false; window.scrollTo({ top: 0 });
    tipografar(corpo);
  }
  function fecharPainel() {
    if (painel.hidden) return;
    abrirAba(vistaAntes);
    window.scrollTo({ top: rolagemAntes });
  }
  document.getElementById('painel-voltar').addEventListener('click', fecharPainel);

  function ativar(ref, viaClique) {
    var el = ancora(ref.dataset.ref);
    if (!el) return;
    if (el.dataset.refTipo === 'exemplo') {
      if (viaClique) abrirGaveta(el.dataset.refNome || 'Exemplo', [el]);
      return;
    }
    if (viaClique) {
      if (fixado && refAtual === ref) { fecharBalao(); return; }
      abrirBalao(ref, el); fixado = true;
    } else if (!fixado) { abrirBalao(ref, el); }
  }

  document.querySelectorAll('.ref').forEach(function (ref) {
    ref.setAttribute('role', 'button'); ref.setAttribute('tabindex', '0');
    ref.addEventListener('click', function (e) { e.stopPropagation(); ativar(ref, true); });
    ref.addEventListener('keydown', function (e) { if (e.key === 'Enter' || e.key === ' ') { e.preventDefault(); ativar(ref, true); } });
    if (temHover) {
      ref.addEventListener('mouseenter', function () { ativar(ref, false); });
      ref.addEventListener('mouseleave', function () { if (!fixado) setTimeout(function () { if (!fixado && !balao.matches(':hover')) fecharBalao(); }, 150); });
    }
  });
  balao.addEventListener('mouseleave', function () { if (!fixado) fecharBalao(); });
  balao.addEventListener('click', function (e) { e.stopPropagation(); });
  gaveta.addEventListener('click', function (e) { e.stopPropagation(); });
  document.addEventListener('click', function () { if (!balao.hidden) fecharBalao(); });
  document.addEventListener('keydown', function (e) { if (e.key === 'Escape') { fecharBalao(); fecharGaveta(); fecharPainel(); } });
  window.addEventListener('resize', fecharBalao);
  document.getElementById('gaveta-fechar').addEventListener('click', fecharGaveta);
  /* Só mostra fórmulas de passos já vistos: referência para frente revelaria conteúdo antes da hora. */
  document.getElementById('btn-formulas').addEventListener('click', function (e) {
    e.stopPropagation(); fecharTrilho();
    var todas = Array.prototype.slice.call(trilha.querySelectorAll('[data-ref-tipo="formula"], [data-ref-tipo="definicao"]'));
    var vistas = todas.filter(function (el) { var i = passoDe(el); return i <= estado.atual || estado.feitos[i]; });
    var resto = todas.length - vistas.length;
    abrirGaveta('Fórmulas até aqui', vistas, resto ? 'Mais ' + resto + ' nos próximos passos.' : '');
  });

  /* ===== Retomada: quem volta depois de um tempo tenta lembrar antes de continuar ===== */
  (function () {
    var box = document.getElementById('retomada');
    if (!box || !voltouDepois) return;
    var comIdeia = passos.map(function (p, i) { return i; }).filter(function (i) { return estado.feitos[i] && passos[i].querySelector('.ideia'); });
    if (!comIdeia.length) return;
    var antes = comIdeia.filter(function (i) { return i < estado.atual; });
    var ultimo = antes.length ? antes[antes.length - 1] : comIdeia[comIdeia.length - 1];
    var alvos = [ultimo];
    var antigos = comIdeia.filter(function (i) { return i < ultimo - 1; });
    if (antigos.length) alvos.unshift(antigos[Math.floor(Math.random() * antigos.length)]);
    var p = document.createElement('p');
    p.textContent = 'Você parou no passo ' + (estado.atual + 1) + '. Antes de continuar, tente lembrar sem olhar:';
    box.appendChild(p);
    alvos.forEach(function (i) {
      var item = document.createElement('div'); item.className = 'item';
      var q = document.createElement('p');
      q.textContent = 'Qual é a ideia central de “' + (passos[i].dataset.titulo || 'Passo') + '” (passo ' + (i + 1) + ')?';
      var b = document.createElement('button'); b.className = 'btn pequeno'; b.setAttribute('data-revelar', ''); b.textContent = 'Ver resposta';
      var r = document.createElement('div'); r.className = 'resposta'; r.hidden = true;
      r.appendChild(copia(passos[i].querySelector('.ideia')));
      item.appendChild(q); item.appendChild(b); item.appendChild(r);
      box.appendChild(item);
    });
    var fechar = document.createElement('button'); fechar.className = 'btn primario'; fechar.textContent = 'Continuar';
    fechar.style.marginTop = '.5rem';
    fechar.addEventListener('click', function () { box.hidden = true; });
    box.appendChild(fechar);
    ligarRevelar(box);
    box.hidden = false;
    tipografar(box);
  })();

  /* ===== Revisão embaralhada (aba Resumo) ===== */
  (function () {
    var box = document.getElementById('revisao');
    if (!box) return;
    var questoes = Array.prototype.slice.call(trilha.querySelectorAll('.quiz'));
    if (!questoes.length) { box.hidden = true; return; }
    var info = box.querySelector('.rev-info'), palco = document.getElementById('rev-palco');
    var btnIniciar = document.getElementById('rev-iniciar'), btnProx = document.getElementById('rev-proxima');
    info.textContent = (questoes.length === 1 ? '1 questão' : questoes.length + ' questões') + ' da trilha, em ordem embaralhada. Responda sem voltar aos passos.';
    var ordem = [], k = 0, acertos = 0;
    function embaralhar(v) { for (var i = v.length - 1; i > 0; i--) { var j = Math.floor(Math.random() * (i + 1)), t = v[i]; v[i] = v[j]; v[j] = t; } return v; }
    function mostrar() {
      palco.innerHTML = ''; btnProx.hidden = true;
      if (k >= ordem.length) {
        var fim = document.createElement('p');
        fim.innerHTML = '<strong>Acertos de primeira: ' + acertos + ' de ' + ordem.length + '.</strong> As que você errou valem uma visita ao passo de origem.';
        palco.appendChild(fim);
        btnIniciar.textContent = 'Embaralhar de novo'; btnIniciar.hidden = false;
        return;
      }
      var original = ordem[k], origem = passoDe(original);
      var cab = document.createElement('p'); cab.className = 'rev-cabeca'; cab.textContent = 'Questão ' + (k + 1) + ' de ' + ordem.length;
      var c = original.cloneNode(true);
      c.querySelectorAll('[id]').forEach(function (x) { x.removeAttribute('id'); });
      c.querySelectorAll('.certa, .errada').forEach(function (x) { x.classList.remove('certa', 'errada'); });
      c.querySelectorAll('.fb').forEach(function (x) { x.classList.remove('mostrar', 'ok', 'no'); });
      palco.appendChild(cab); palco.appendChild(c);
      var irOrigem = document.createElement('button'); irOrigem.className = 'ir-passo'; irOrigem.hidden = true;
      irOrigem.textContent = 'Rever o passo ' + (origem + 1);
      irOrigem.addEventListener('click', function () { abrirAba('trilha'); ir(origem); });
      palco.appendChild(irOrigem);
      ligarQuiz(c, function (certa, primeira) {
        if (primeira && certa) acertos++;
        irOrigem.hidden = false;
        btnProx.hidden = false;
        btnProx.textContent = k === ordem.length - 1 ? 'Ver resultado' : 'Próxima';
      });
      tipografar(palco);
    }
    btnIniciar.addEventListener('click', function () { ordem = embaralhar(questoes.slice()); k = 0; acertos = 0; btnIniciar.hidden = true; mostrar(); });
    btnProx.addEventListener('click', function () { k++; mostrar(); });
  })();

  /* ===== Integração com o site do livro (se houver assets/site.js) ===== */
  (function () {
    var site = window.SITE;
    if (!site) return;
    var inicio = document.getElementById('btn-inicio');
    if (inicio) inicio.hidden = false;
    var caps = site.capitulos || [], k = -1;
    caps.forEach(function (c, i) { if (site.slug + '/' + c.id === idPagina) k = i; });
    if (k < 0) return;
    var prox = null;
    for (var i = k + 1; i < caps.length; i++) if (caps[i].status === 'pronto') { prox = caps[i]; break; }
    var box = document.createElement('div'); box.className = 'fim-capitulo';
    box.innerHTML = '<a class="btn" href="../index.html">← Início</a>' +
      (prox ? '<a class="btn primario" href="../' + prox.arquivo + '">Capítulo ' + prox.numero + ': ' + prox.titulo + ' →</a>' : '');
    resumo.appendChild(box);
  })();

  render();
  salvar();
})();

/* ===== Utilitários para simulações em SVG ===== */
function svgEl(tag, attrs, texto) {
  var el = document.createElementNS('http://www.w3.org/2000/svg', tag);
  for (var k in attrs) el.setAttribute(k, attrs[k]);
  if (texto !== undefined) el.textContent = texto;
  return el;
}
/* Grupo recortado num retângulo: o que passar dele some (ex.: retas que atravessam o gráfico).
   Chame depois de limpar(svg) e desenhe dentro do grupo devolvido. */
function recorte(svg, x, y, w, h) {
  var id = 'recorte-' + (recorte.n = (recorte.n || 0) + 1), cp = svgEl('clipPath', { id: id }), defs = svgEl('defs', {});
  cp.appendChild(svgEl('rect', { x: x, y: y, width: w, height: h })); defs.appendChild(cp); svg.appendChild(defs);
  var g = svgEl('g', { 'clip-path': 'url(#' + id + ')' }); svg.appendChild(g); return g;
}
function corVar(nome) { return getComputedStyle(document.documentElement).getPropertyValue(nome).trim(); }
/* Número com vírgula decimal e sinal de menos tipográfico (−0,93). */
function fmt(v, d) { var t = v.toFixed(d); if (/^-0[,.]?0*$/.test(t)) t = t.slice(1); return t.replace('.', ',').replace('-', '−'); }

