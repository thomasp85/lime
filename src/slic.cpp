#include <Rcpp.h>

using namespace Rcpp;

std::pair<int, int> local_minimum(RawMatrix L, int x, int y) {
  int min_grad = INT_MAX;
  std::pair<int, int> loc_min(x, y);
  int c1, c2, c3, grad;

  for (int i = x-1; i < x+2; i++) {
    for (int j = y-1; j < y+2; j++) {
      c1 = L(j+1, i);
      c2 = L(j, i+1);
      c3 = L(j, i);
      grad = std::abs(c1 - c3) + std::abs(c2 - c3);
      if (grad < min_grad) {
        min_grad = grad;
        loc_min.first = i;
        loc_min.second = j;
      }
    }
  }

  return loc_min;
}

double pixel_dist(int x, int y, int L, int a, int b, std::vector<double> center, double step, double weight) {
  int Ld = center[0] - L;
  int ad = center[1] - a;
  int bd = center[2] - b;
  int xd = center[3] - x;
  int yd = center[4] - y;
  double dc = std::sqrt(Ld*Ld + ad*ad + bd*bd) / weight;
  double ds = std::sqrt(xd*xd + yd*yd) / step;
  return std::sqrt(dc*dc + ds*ds);
}

IntegerMatrix connect_pixels(IntegerMatrix cluster, int n_centers) {
  int label = 0, adjlabel = 0;
  int width = cluster.ncol();
  int height = cluster.nrow();
  int lims = (width * height) / n_centers;

  int dx4[4] = {-1,  0,  1,  0};
  int dy4[4] = { 0, -1,  0,  1};

  IntegerMatrix new_cluster(height, width);
  std::fill(new_cluster.begin(), new_cluster.end(), -1);

  for (int i = 0; i < width; i++) {
    for (int j = 0; j < height; j++) {
      if (new_cluster(j, i) == -1) {
        std::vector< std::pair<int, int> > elements;
        std::pair<int, int> point(i, j);
        elements.push_back(point);

        /* Find an adjacent label, for possible use later. */
        for (int k = 0; k < 4; k++) {
          int x = elements[0].first + dx4[k], y = elements[0].second + dy4[k];

          if (x >= 0 && x < width && y >= 0 && y < height) {
            if (new_cluster(y, x) >= 0) {
              adjlabel = new_cluster(y, x);
            }
          }
        }

        int count = 1;
        for (int c = 0; c < count; c++) {
          for (int k = 0; k < 4; k++) {
            int x = elements[c].first + dx4[k], y = elements[c].second + dy4[k];

            if (x >= 0 && x < width && y >= 0 && y < height) {
              if (new_cluster(y, x) == -1 && cluster(j, i) == cluster(y, x)) {
                std::pair<int, int> new_point(x, y);
                elements.push_back(new_point);
                new_cluster(y, x) = label;
                count += 1;
              }
            }
          }
        }

        /* Use the earlier found adjacent label if a segment size is
        smaller than a limit. */
        if (count <= lims >> 2) {
          for (int c = 0; c < count; c++) {
            new_cluster(elements[c].second, elements[c].first) = adjlabel;
          }
          label -= 1;
        }
        label += 1;
      }
    }
  }
  return new_cluster;
}

//' @title Segment image into superpixels
//'
//' @description This is an implementation of the SLIC superpixel algorithm for
//' segmenting images into connected similar patches. It is used by lime for
//' permuting image input but exported so that others might use it for other
//' things
//'
//' @param L,a,b Raw matrices giving the L, a, and b component of each pixel in
//' the image to segment. The dimensions of all matrices must match.
//'
//' @param n_sp The number of superpixels to segment the image into
//'
//' @param weight A numeric giving the tradeoff between spatial and colour
//' distance. Higher values give more compact and heterogeneous superpixels,
//' while lower values will give superpixels of more irregular shape but with a
//' more homogeneous colour. Good values to start with is 10-20.
//'
//' @param n_iter The number of iterations to run the algorithm for. The authors
//' suggest 10 and increasing it doesn't add much.
//'
//' @return An integer matrix of the same dimensions as `L`, `a`, and `b`,
//' indexing each pixel into its corresponding superpixel
//'
//' @keywords internal
//' @export
// [[Rcpp::export]]
IntegerMatrix slic(RawMatrix L, RawMatrix a, RawMatrix b, int n_sp, double weight, int n_iter) {
  int w = L.ncol();
  int h = L.nrow();
  IntegerMatrix cluster(h, w);
  std::fill(cluster.begin(), cluster.end(), -1);
  NumericMatrix distances(h, w);
  std::vector< std::vector<double> > centers;
  NumericVector center_counts;
  double step = std::sqrt((w * h) / (double) n_sp);
  for (int x = step/2; x < w - step/2; x += step) {
    for (int y = step/2; y < h - step/2; y += step) {
      std::vector<double> center;
      std::pair<int, int> center_pos = local_minimum(L, x, y);

      center.push_back(L(center_pos.second, center_pos.first));
      center.push_back(a(center_pos.second, center_pos.first));
      center.push_back(b(center_pos.second, center_pos.first));
      center.push_back(center_pos.first);
      center.push_back(center_pos.second);

      centers.push_back(center);
      center_counts.push_back(0);
    }
  }
  for (int i = 0; i < n_iter; i++) {
    std::fill(distances.begin(), distances.end(), R_PosInf);

    for (int j = 0; j < centers.size(); j++) {
      for (int x = centers[j][3] - step; x < centers[j][3] + step; x++) {
        for (int y = centers[j][4] - step; y < centers[j][4] + step; y++) {

          if (x >= 0 && x < w && y >= 0 && y < h) {
            double d = pixel_dist(x, y, L(y, x), a(y, x), b(y, x), centers[j], step, weight);
            if (d < distances(y, x)) {
              distances(y, x) = d;
              cluster(y, x) = j;
            }
          }
        }
      }
      std::fill(centers[j].begin(), centers[j].end(), 0);
      center_counts[j] = 0;
    }
    for (int x = 0; x < w; x++) {
      for (int y = 0; y < h; y++) {
        int j = cluster(y, x);

        if (j != -1) {
          centers[j][0] += L(y, x);
          centers[j][1] += a(y, x);
          centers[j][2] += b(y, x);
          centers[j][3] += x;
          centers[j][4] += y;

          center_counts[j] += 1;
        }
      }
    }
    for (int j = 0; j < centers.size(); j++) {
      centers[j][0] /= center_counts[j];
      centers[j][1] /= center_counts[j];
      centers[j][2] /= center_counts[j];
      centers[j][3] /= center_counts[j];
      centers[j][4] /= center_counts[j];
    }
  }
  IntegerMatrix connected_cluster = connect_pixels(cluster, centers.size());

  return connected_cluster;
}
