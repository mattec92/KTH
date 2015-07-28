package se.mattec.id2208.project;

/**
 * Copyright 2010 Websoft research group, Nanjing University, PR China
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); 
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 *     
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * @author Wei Hu
 * @see http://ws.nju.edu.cn
 */
public class EditDistance
{
    public static int getEditDistance(String s, String t)
    {
        int d[][];
        int n;
        int m;
        int i;
        int j;
        char s_i;
        char t_j;
        int cost;

        n = s.length();
        m = t.length();
        if (n == 0) {
            return m;
        }
        if (m == 0) {
            return n;
        }
        d = new int[n + 1][m + 1];
        for (i = 0; i <= n; i++) {
            d[i][0] = i;

        }
        for (j = 0; j <= m; j++) {
            d[0][j] = j;

        }
        for (i = 1; i <= n; i++) {
            s_i = s.charAt(i - 1);
            for (j = 1; j <= m; j++) {
                t_j = t.charAt(j - 1);
                if (s_i == t_j) {
                    cost = 0;
                } else {
                    cost = 1;
                }
                d[i][j] = Minimum(
                		d[i - 1][j] + 1, d[i][j - 1] + 1, d[i - 1][j - 1] + cost);
            }
        }
        return d[n][m];

    }

    private static int Minimum(int a, int b, int c)
    {
        int mi;
        mi = a;
        if (b < mi) {
            mi = b;
        }
        if (c < mi) {
            mi = c;
        }
        return mi;
    }

    public static double getSimilarity(String s, String t)
    {
        int edit = EditDistance.getEditDistance(s, t);
        double sim = 1 / Math.exp(edit / (double) (s.length() + t.length() - edit));
        return sim;
    }
}
