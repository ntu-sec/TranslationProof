public class SampleCDR
{
  public void sample (int a, int b)
  {
    int c = 0;
    for (; a > 0; a--)
    {
      c = c + b;
      if (a % 2 == 0) c = c - a;
    }
  }
}
