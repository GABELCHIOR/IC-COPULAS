/* Modo guia: o site da IC é visitado a partir do banner, então por padrão funciona como guia de leitura.
   Simulações livres, perguntas com a resposta aberta, contas como exemplos resolvidos e sem questões de prova.
   O modo estudo (com todos os exercícios) fica a um clique e é lembrado neste navegador.
   Carregar depois do estudo.js. */
(function () {
  var CHAVE = 'ic-copulas:modo';
  var modo = 'guia';
  try { modo = localStorage.getItem(CHAVE) === 'estudo' ? 'estudo' : 'guia'; } catch (e) {}
  var guia = modo === 'guia';
  document.documentElement.classList.toggle('modo-guia', guia);

  function trocar() {
    try { localStorage.setItem(CHAVE, guia ? 'estudo' : 'guia'); } catch (e) {}
    location.reload();
  }
  function interruptor() {
    var box = document.createElement('div');
    box.className = 'modo-uso';
    var txt = document.createElement('span');
    txt.textContent = guia ? 'Modo guia: leitura com simulações, sem exercícios.' : 'Modo estudo: com exercícios e questões.';
    var b = document.createElement('button');
    b.type = 'button';
    b.textContent = guia ? 'Mostrar exercícios' : 'Voltar ao modo guia';
    b.addEventListener('click', trocar);
    box.appendChild(txt);
    box.appendChild(b);
    return box;
  }

  // ---- página inicial: só o interruptor e o link das questões
  var trilha = document.getElementById('trilha');
  if (!trilha) {
    var nav = document.querySelector('.site-nav');
    if (nav) {
      nav.querySelectorAll('a[href="revisao.html"]').forEach(function (a) { if (guia) a.classList.add('so-estudo'); });
      nav.parentNode.insertBefore(interruptor(), nav.nextSibling);
    }
    return;
  }

  // ---- capítulos
  var abas = document.querySelector('.abas');
  if (abas) abas.parentNode.insertBefore(interruptor(), abas.nextSibling);
  if (!guia) return;

  // Preveja: simulação liberada; a pergunta fica como guia e a explicação aparece
  trilha.querySelectorAll('.prever').forEach(function (pv) {
    var sim = pv.dataset.sim ? document.getElementById(pv.dataset.sim) : null;
    if (sim) { sim.classList.remove('travada'); sim.inert = false; }
    pv.querySelectorAll('.alts, .aviso-prever, button.btn').forEach(function (x) { x.classList.add('so-estudo'); });
    var ex = pv.querySelector('.explica');
    if (ex) ex.hidden = false;
  });

  // Pare e tente: resposta aberta, sem botão
  trilha.querySelectorAll('.pare').forEach(function (pt) {
    pt.querySelectorAll('[data-revelar]').forEach(function (b) { b.classList.add('so-estudo'); });
    var r = pt.querySelector('.resposta');
    if (r) r.hidden = false;
  });

  // Complete: vira exemplo resolvido (enunciado + resolução)
  trilha.querySelectorAll('.lacunas').forEach(function (bx) {
    bx.querySelectorAll(':scope > p').forEach(function (p) {
      if (p.querySelector('input.lacuna') || p.classList.contains('dica') || p.classList.contains('res-lacunas')) { p.classList.add('so-estudo'); return; }
      p.innerHTML = p.innerHTML.replace(/\s*\(\d+ casas?\)/g, '').replace(/^Complete a /, 'Para a ');
    });
    bx.querySelectorAll(':scope > button').forEach(function (b) { b.classList.add('so-estudo'); });
    var res = bx.querySelector('.resolucao');
    if (res) res.hidden = false;
  });

  // Ache o erro: mostra a linha errada e por quê
  trilha.querySelectorAll('.acheerro').forEach(function (ae) {
    var intro = ae.querySelector(':scope > p');
    if (intro) intro.innerHTML = intro.innerHTML.replace(/\s*Toque na (linha|passo) com erro\.?/, '');
    ae.querySelectorAll('.linhas button').forEach(function (b) { b.disabled = true; });
    var errada = ae.querySelector('.linhas li[data-errada]');
    if (errada) {
      var p = document.createElement('p');
      p.className = 'por-que';
      p.textContent = 'Linha ' + (Array.prototype.indexOf.call(errada.parentNode.children, errada) + 1) + ': ' + (errada.dataset.fb || '');
      ae.appendChild(p);
    }
  });

  // Questões de múltipla escolha: fora do modo guia
  trilha.querySelectorAll('.quiz').forEach(function (q) { q.classList.add('so-estudo'); });

  // Coluna de apoio que ficou vazia: o passo vira coluna única
  trilha.querySelectorAll('.passo > .apoio').forEach(function (a) {
    var visiveis = Array.prototype.some.call(a.children, function (c) { return getComputedStyle(c).display !== 'none'; });
    if (!visiveis) { a.classList.add('so-estudo'); a.parentNode.classList.remove('com-apoio'); }
  });

  // A aba Resumo perde a revisão embaralhada; o texto do resumo fica
  var rev = document.getElementById('revisao');
  if (rev) rev.hidden = true;
})();
