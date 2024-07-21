// Classe Estrela
class Estrela {
  float x, y;

  Estrela(float x1, float y1) {
    x = x1;
    y = y1;
  }

  void desenhar() {
    stroke(255); // Cor branca
    point(x, y); // Desenha uma estrela
  }
}
