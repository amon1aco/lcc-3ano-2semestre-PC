class Sol {
  float x, y;
  
  Sol(float x, float y) {
    this.x = x;
    this.y = y;
  }
  
  void display() {
    fill(255, 255, 0);
    ellipse(x, y, 100, 100);
  }
}
