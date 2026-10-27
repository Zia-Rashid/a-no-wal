%% args: check --dump=ast --annot=idx
type shape = circle(kl) | rect(kl, kl)

otala area(shape s) -> kl {
  katsa (s) {
    circle(r) => 3.14 * r * r,
    rect(w, h) => w * h,
  }
}

shapes: [circle(1.0), rect(2.0, 3.0)]
total: for (s of shapes) with (kl acc: 0.0) { acc + area(s) }
big: filter(sizes, _ > 2.0)
sign: n < 0 ? "neg" : n == 0 ? "zero" : "pos"
p: <x: 1, y: 2, far: (total > 100.0)>
ok: 0 <= i < shapes.sz
