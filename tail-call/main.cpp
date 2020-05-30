#include <iostream>

using namespace std;

struct state
{
  int currentSum;
  int *list;
  int size;
};

int sum_iteration(int *list, int size)
{
  struct state state;
  state.currentSum = 0;
  state.list = list;
  state.size = size;

  while (true) {
    if (state.size == 0)
      return state.currentSum;
    else {
      state.currentSum += list[0];
      state.list += 1;
      state.size -= 1;
    }
  }
}


int main()
{
  int list[] = {2, 4, 3};
  int size = 3;
  cout << sum_iteration(list, size) << endl;

  return 0;
}
