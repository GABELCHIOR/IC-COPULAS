/* Funções estatísticas usadas pelas simulações do site (distribuições, cópulas, sorteios).
   Tudo em window.CL. Sem dependências. */
(function () {
  var CL = {};

  /* ---------- gerador com semente (mulberry32) ---------- */
  CL.rng = function (semente) {
    var a = semente >>> 0;
    return function () {
      a = (a + 0x6D2B79F5) >>> 0;
      var t = a;
      t = Math.imul(t ^ (t >>> 15), t | 1);
      t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
      return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
    };
  };
  CL.rnorm = function (r) {
    var u = 0, v = 0;
    while (u === 0) u = r();
    v = r();
    return Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v);
  };
  /* gama(forma, 1) de Marsaglia e Tsang */
  CL.rgamma = function (r, k) {
    if (k < 1) return CL.rgamma(r, k + 1) * Math.pow(r(), 1 / k);
    var d = k - 1 / 3, c = 1 / Math.sqrt(9 * d);
    for (;;) {
      var x, v;
      do { x = CL.rnorm(r); v = 1 + c * x; } while (v <= 0);
      v = v * v * v;
      var u = r();
      if (u < 1 - 0.0331 * x * x * x * x) return d * v;
      if (Math.log(u) < 0.5 * x * x + d * (1 - v + Math.log(v))) return d * v;
    }
  };

  /* ---------- funções especiais ---------- */
  CL.lgamma = function (x) {
    var c = [76.18009172947146, -86.50532032941677, 24.01409824083091,
      -1.231739572450155, 0.1208650973866179e-2, -0.5395239384953e-5];
    var y = x, t = x + 5.5;
    t -= (x + 0.5) * Math.log(t);
    var s = 1.000000000190015;
    for (var j = 0; j < 6; j++) s += c[j] / ++y;
    return -t + Math.log(2.5066282746310005 * s / x);
  };
  function betacf(a, b, x) {
    var qab = a + b, qap = a + 1, qam = a - 1, c = 1, d = 1 - qab * x / qap;
    if (Math.abs(d) < 1e-300) d = 1e-300;
    d = 1 / d;
    var h = d;
    for (var m = 1; m <= 300; m++) {
      var m2 = 2 * m, aa = m * (b - m) * x / ((qam + m2) * (a + m2));
      d = 1 + aa * d; if (Math.abs(d) < 1e-300) d = 1e-300;
      c = 1 + aa / c; if (Math.abs(c) < 1e-300) c = 1e-300;
      d = 1 / d; h *= d * c;
      aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2));
      d = 1 + aa * d; if (Math.abs(d) < 1e-300) d = 1e-300;
      c = 1 + aa / c; if (Math.abs(c) < 1e-300) c = 1e-300;
      d = 1 / d;
      var del = d * c; h *= del;
      if (Math.abs(del - 1) < 3e-14) break;
    }
    return h;
  }
  CL.ibeta = function (x, a, b) {
    if (x <= 0) return 0;
    if (x >= 1) return 1;
    var bt = Math.exp(CL.lgamma(a + b) - CL.lgamma(a) - CL.lgamma(b) + a * Math.log(x) + b * Math.log(1 - x));
    if (x < (a + 1) / (a + b + 2)) return bt * betacf(a, b, x) / a;
    return 1 - bt * betacf(b, a, 1 - x) / b;
  };

  /* ---------- normal ---------- */
  CL.pnorm = function (z) {
    // erfc de Numerical Recipes (erro relativo < 1,2e-7), suficiente para os gráficos
    var x = Math.abs(z) / Math.SQRT2, t = 1 / (1 + 0.5 * x);
    var y = t * Math.exp(-x * x - 1.26551223 + t * (1.00002368 + t * (0.37409196 + t * (0.09678418 +
      t * (-0.18628806 + t * (0.27886807 + t * (-1.13520398 + t * (1.48851587 + t * (-0.82215223 + t * 0.17087277)))))))));
    // y = erfc(|z|/√2)
    return z >= 0 ? 1 - y / 2 : y / 2;
  };
  CL.dnorm = function (z) { return Math.exp(-z * z / 2) / 2.5066282746310002; };
  CL.qnorm = function (p) {
    if (p <= 0) return -Infinity;
    if (p >= 1) return Infinity;
    var a = [-3.969683028665376e+01, 2.209460984245205e+02, -2.759285104469687e+02, 1.383577518672690e+02, -3.066479806614716e+01, 2.506628277459239e+00],
      b = [-5.447609879822406e+01, 1.615858368580409e+02, -1.556989798598866e+02, 6.680131188771972e+01, -1.328068155288572e+01],
      c = [-7.784894002430293e-03, -3.223964580411365e-01, -2.400758277161838e+00, -2.549732539343734e+00, 4.374664141464968e+00, 2.938163982698783e+00],
      d = [7.784695709041462e-03, 3.224671290700398e-01, 2.445134137142996e+00, 3.754408661907416e+00];
    var q, r, x;
    if (p < 0.02425) {
      q = Math.sqrt(-2 * Math.log(p));
      x = (((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5]) / ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1);
    } else if (p > 1 - 0.02425) {
      q = Math.sqrt(-2 * Math.log(1 - p));
      x = -(((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5]) / ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1);
    } else {
      q = p - 0.5; r = q * q;
      x = (((((a[0] * r + a[1]) * r + a[2]) * r + a[3]) * r + a[4]) * r + a[5]) * q / (((((b[0] * r + b[1]) * r + b[2]) * r + b[3]) * r + b[4]) * r + 1);
    }
    return x;
  };

  /* ---------- t de Student ---------- */
  CL.pt = function (x, nu) {
    var p = 0.5 * CL.ibeta(nu / (nu + x * x), nu / 2, 0.5);
    return x > 0 ? 1 - p : p;
  };
  CL.qt = function (p, nu) {
    if (p <= 0) return -Infinity;
    if (p >= 1) return Infinity;
    var lo = -1, hi = 1;
    while (CL.pt(lo, nu) > p) lo *= 2;
    while (CL.pt(hi, nu) < p) hi *= 2;
    for (var i = 0; i < 80; i++) {
      var m = (lo + hi) / 2;
      if (CL.pt(m, nu) < p) lo = m; else hi = m;
    }
    return (lo + hi) / 2;
  };
  CL.dt = function (x, nu) {
    return Math.exp(CL.lgamma((nu + 1) / 2) - CL.lgamma(nu / 2)) / Math.sqrt(nu * Math.PI) * Math.pow(1 + x * x / nu, -(nu + 1) / 2);
  };

  /* ---------- marginais do trabalho (em log-retorno, não em %) ---------- */
  // t padronizada (média m, desvio-padrão s, nu graus de liberdade)
  CL.margT = function (m, s, nu) {
    var esc = s * Math.sqrt((nu - 2) / nu);
    return {
      p: function (x) { return CL.pt((x - m) / esc, nu); },
      q: function (u) { return m + esc * CL.qt(u, nu); },
      d: function (x) { return CL.dt((x - m) / esc, nu) / esc; }
    };
  };
  // Johnson SU: gamma + delta * asinh((y - xi) / lambda) é normal padrão
  CL.margJSU = function (g, dl, xi, la) {
    return {
      p: function (y) { return CL.pnorm(g + dl * Math.asinh((y - xi) / la)); },
      q: function (u) { return xi + la * Math.sinh((CL.qnorm(u) - g) / dl); },
      d: function (y) { var z = (y - xi) / la; return dl / (la * Math.sqrt(1 + z * z)) * CL.dnorm(g + dl * Math.asinh(z)); }
    };
  };

  /* ---------- cópulas ---------- */
  var FAM = {
    clayton: { C: function (u, v, t) { return Math.pow(Math.max(Math.pow(u, -t) + Math.pow(v, -t) - 1, 0), -1 / t); },
      tau: function (t) { return t / (t + 2); } },
    gumbel: { C: function (u, v, t) { return Math.exp(-Math.pow(Math.pow(-Math.log(u), t) + Math.pow(-Math.log(v), t), 1 / t)); },
      tau: function (t) { return 1 - 1 / t; } },
    frank: { C: function (u, v, t) {
        if (Math.abs(t) < 1e-9) return u * v;
        return -1 / t * Math.log(1 + (Math.exp(-t * u) - 1) * (Math.exp(-t * v) - 1) / (Math.exp(-t) - 1));
      },
      tau: function (t) {
        if (Math.abs(t) < 1e-9) return 0;
        var at = Math.abs(t), n = 400, s = 0;
        for (var i = 1; i <= n; i++) { var x = (i - 0.5) / n * at; s += x / (Math.exp(x) - 1); }
        var D1 = s * (at / n) / at;
        var tau = 1 - 4 / at * (1 - D1);
        return t < 0 ? -tau : tau;
      } },
    joe: { C: function (u, v, t) {
        var a = Math.pow(1 - u, t), b = Math.pow(1 - v, t);
        return 1 - Math.pow(a + b - a * b, 1 / t);
      },
      tau: function (t) {
        // tau = 1 + 4 * integral de phi/phi' em (0,1), phi(s) = -ln(1 - (1 - s)^t)
        var n = 2000, s = 0;
        for (var i = 1; i <= n; i++) {
          var x = (i - 0.5) / n, w = Math.pow(1 - x, t);
          var phi = -Math.log(1 - w), dphi = -t * Math.pow(1 - x, t - 1) / (1 - w);
          s += phi / dphi;
        }
        return 1 + 4 * s / n;
      } },
    amh: { C: function (u, v, t) { return u * v / (1 - t * (1 - u) * (1 - v)); },
      tau: function (t) {
        if (Math.abs(t) < 1e-9) return 0;
        var l = Math.log(1 - t);
        return 1 - 2 * (t + (1 - t) * (1 - t) * l) / (3 * t * t);
      } }
  };
  CL.familias = FAM;

  // ∂C/∂u por diferença central (com bordas tratadas)
  function dCdu(C, u, v, t) {
    var e = 1e-6, a = Math.max(u - e, 1e-12), b = Math.min(u + e, 1 - 1e-12);
    return (C(b, v, t) - C(a, v, t)) / (b - a);
  }
  // sorteio por inversão condicional: u uniforme, depois v que resolve ∂C/∂u(u, v) = w
  CL.sortearArq = function (fam, t, n, r) {
    var C = FAM[fam].C, out = [];
    for (var i = 0; i < n; i++) {
      var u = Math.min(Math.max(r(), 1e-9), 1 - 1e-9), w = r(), lo = 1e-12, hi = 1 - 1e-12;
      for (var k = 0; k < 50; k++) {
        var m = (lo + hi) / 2;
        if (dCdu(C, u, m, t) < w) lo = m; else hi = m;
      }
      out.push([u, (lo + hi) / 2]);
    }
    return out;
  };
  CL.sortearNormal = function (rho, n, r) {
    var out = [], s = Math.sqrt(1 - rho * rho);
    for (var i = 0; i < n; i++) {
      var z1 = CL.rnorm(r), z2 = rho * z1 + s * CL.rnorm(r);
      out.push([CL.pnorm(z1), CL.pnorm(z2)]);
    }
    return out;
  };
  CL.sortearT = function (rho, nu, n, r) {
    var out = [], s = Math.sqrt(1 - rho * rho);
    for (var i = 0; i < n; i++) {
      var z1 = CL.rnorm(r), z2 = rho * z1 + s * CL.rnorm(r);
      var w = Math.sqrt(nu / (2 * CL.rgamma(r, nu / 2)));
      out.push([CL.pt(z1 * w, nu), CL.pt(z2 * w, nu)]);
    }
    return out;
  };
  // espelhamentos: 'v' troca v por 1 - v; 'u' troca u por 1 - u
  CL.espelhar = function (pares, eixo) {
    return pares.map(function (p) { return eixo === 'v' ? [p[0], 1 - p[1]] : eixo === 'u' ? [1 - p[0], p[1]] : p; });
  };

  // condicional da cópula t: P(U <= u | V = v)
  CL.hT = function (u, v, rho, nu) {
    var a = CL.qt(u, nu), b = CL.qt(v, nu);
    return CL.pt((a - rho * b) / Math.sqrt((nu + b * b) * (1 - rho * rho) / (nu + 1)), nu + 1);
  };
  // inversa da condicional da cópula t (em u)
  CL.hTinv = function (w, v, rho, nu) {
    var b = CL.qt(v, nu);
    var a = CL.qt(w, nu + 1) * Math.sqrt((nu + b * b) * (1 - rho * rho) / (nu + 1)) + rho * b;
    return CL.pt(a, nu);
  };
  // dependência de cauda da t: canto da diagonal (mesmo sinal) e fora dela
  CL.lambdaT = function (rho, nu, foraDiagonal) {
    var r = foraDiagonal ? -rho : rho;
    return 2 * CL.pt(-Math.sqrt((nu + 1) * (1 - r) / (1 + r)), nu + 1);
  };
  CL.tauEliptica = function (rho) { return 2 / Math.PI * Math.asin(rho); };

  /* τ de Kendall amostral (O(n²)) */
  CL.kendall = function (a, b) {
    var c = 0, d = 0, n = a.length;
    for (var i = 0; i < n; i++) for (var j = i + 1; j < n; j++) {
      var s = (a[i] - a[j]) * (b[i] - b[j]);
      if (s > 0) c++; else if (s < 0) d++;
    }
    return (c - d) / (n * (n - 1) / 2);
  };
  CL.pearson = function (a, b) {
    var n = a.length, ma = 0, mb = 0;
    for (var i = 0; i < n; i++) { ma += a[i]; mb += b[i]; }
    ma /= n; mb /= n;
    var sab = 0, saa = 0, sbb = 0;
    for (i = 0; i < n; i++) { sab += (a[i] - ma) * (b[i] - mb); saa += (a[i] - ma) * (a[i] - ma); sbb += (b[i] - mb) * (b[i] - mb); }
    return sab / Math.sqrt(saa * sbb);
  };
  CL.postos = function (a) {
    var idx = a.map(function (x, i) { return i; }).sort(function (i, j) { return a[i] - a[j]; });
    var r = new Array(a.length);
    idx.forEach(function (k, pos) { r[k] = (pos + 1) / (a.length + 1); });
    return r;
  };
  CL.quantil = function (arr, p) {
    var s = arr.slice().sort(function (x, y) { return x - y; });
    var h = (s.length - 1) * p, i = Math.floor(h);
    return s[i] + (h - i) * ((s[Math.min(i + 1, s.length - 1)]) - s[i]);
  };

  /* ---------- densidades das cópulas elípticas ---------- */
  CL.cT = function (u, v, rho, nu) {
    var a = CL.qt(u, nu), b = CL.qt(v, nu), r2 = 1 - rho * rho;
    var lg = CL.lgamma((nu + 2) / 2) - CL.lgamma(nu / 2) - Math.log(nu * Math.PI * Math.sqrt(r2));
    var f2 = Math.exp(lg) * Math.pow(1 + (a * a - 2 * rho * a * b + b * b) / (nu * r2), -(nu + 2) / 2);
    return f2 / (CL.dt(a, nu) * CL.dt(b, nu));
  };
  CL.cNormal = function (u, v, rho) {
    var a = CL.qnorm(u), b = CL.qnorm(v), r2 = 1 - rho * rho;
    return Math.exp(-(rho * rho * (a * a + b * b) - 2 * rho * a * b) / (2 * r2)) / Math.sqrt(r2);
  };
  CL.hNormal = function (u, v, rho) {
    return CL.pnorm((CL.qnorm(u) - rho * CL.qnorm(v)) / Math.sqrt(1 - rho * rho));
  };
  // C(u, v) = integral de 0 a v de h(u | s) ds, por pontos médios em escala adequada
  CL.Cporh = function (h, u, v, n) {
    n = n || 400;
    var s = 0;
    for (var i = 0; i < n; i++) s += h(u, (i + 0.5) / n * v);
    return s * v / n;
  };

  /* ---------- gráficos em SVG (usam svgEl e corVar do estudo.js) ---------- */
  CL.limpar = function (svg) { while (svg.firstChild) svg.removeChild(svg.firstChild); };
  // o = {x, y, w, h, xmin, xmax, ymin, ymax, xticks, yticks, xfmt, yfmt, xlab, ylab, grade}
  CL.eixos = function (svg, o) {
    var sx = function (v) { return o.x + (v - o.xmin) / (o.xmax - o.xmin) * o.w; };
    var sy = function (v) { return o.y + o.h - (v - o.ymin) / (o.ymax - o.ymin) * o.h; };
    var mut = corVar('--muted'), borda = corVar('--border');
    var fx = o.xfmt || function (v) { return fmt(v, 0); }, fy = o.yfmt || function (v) { return fmt(v, 0); };
    (o.xticks || []).forEach(function (t) {
      if (o.grade) svg.appendChild(svgEl('line', { x1: sx(t), x2: sx(t), y1: o.y, y2: o.y + o.h, stroke: borda, 'stroke-width': 1 }));
      svg.appendChild(svgEl('line', { x1: sx(t), x2: sx(t), y1: o.y + o.h, y2: o.y + o.h + 4, stroke: mut }));
      svg.appendChild(svgEl('text', { x: sx(t), y: o.y + o.h + 16, 'font-size': 11, 'text-anchor': 'middle', fill: mut }, fx(t)));
    });
    (o.yticks || []).forEach(function (t) {
      if (o.grade) svg.appendChild(svgEl('line', { x1: o.x, x2: o.x + o.w, y1: sy(t), y2: sy(t), stroke: borda, 'stroke-width': 1 }));
      svg.appendChild(svgEl('line', { x1: o.x - 4, x2: o.x, y1: sy(t), y2: sy(t), stroke: mut }));
      svg.appendChild(svgEl('text', { x: o.x - 7, y: sy(t) + 4, 'font-size': 11, 'text-anchor': 'end', fill: mut }, fy(t)));
    });
    svg.appendChild(svgEl('line', { x1: o.x, x2: o.x + o.w, y1: o.y + o.h, y2: o.y + o.h, stroke: mut }));
    svg.appendChild(svgEl('line', { x1: o.x, x2: o.x, y1: o.y, y2: o.y + o.h, stroke: mut }));
    if (o.xlab) svg.appendChild(svgEl('text', { x: o.x + o.w / 2, y: o.y + o.h + 32, 'font-size': 11.5, 'text-anchor': 'middle', fill: mut }, o.xlab));
    if (o.ylab) {
      var t = svgEl('text', { x: 0, y: 0, 'font-size': 11.5, 'text-anchor': 'middle', fill: mut, transform: 'translate(' + (o.x - 34) + ',' + (o.y + o.h / 2) + ') rotate(-90)' }, o.ylab);
      svg.appendChild(t);
    }
    var g = recorte(svg, o.x, o.y, o.w, o.h);
    return { sx: sx, sy: sy, g: g };
  };
  CL.caminho = function (pts) {
    return pts.map(function (p, i) { return (i ? 'L' : 'M') + p[0].toFixed(2) + ',' + p[1].toFixed(2); }).join(' ');
  };

  window.CL = CL;
})();
