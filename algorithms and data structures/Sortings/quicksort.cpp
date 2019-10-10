#include <iostream>
#include <vector>
 
using namespace std;
 
int n;
vector<int> input;
 
int partition(int l, int r){
    int v = input[(l + r) / 2];
    int i = l;
    int j = r;
    while (i <= j){
        while (input[i] < v){
            i++;
        }
        while (input[j] > v){
            j--;
        }
        if (i >= j){
            break;
        }
        swap(input[i], input[j]);
        i++;
        j--;
    }
    return j;
}
 
void qsort(int l, int r){
    if (l < r){
        int q = partition(l, r);
        qsort(l, q);
        qsort(q + 1, r);
    }
}
 
int main() {
    cin >> n;
    input.resize(n);
 
    for (int i = 0; i < n; i++){
        cin >> input[i];
    }
    qsort(0, input.size() - 1);
    for (int i = 0; i < n; i++){
        cout << input[i] << " ";
    }
    return 0;
}