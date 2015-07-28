/*
 *  Main author:
 *     Christian Schulte <cschulte@kth.se>
 *
 *  Copyright:
 *     Christian Schulte, 2009
 *
 *  Permission is hereby granted, free of charge, to any person obtaining
 *  a copy of this software and associated documentation files (the
 *  "Software"), to deal in the Software without restriction, including
 *  without limitation the rights to use, copy, modify, merge, publish,
 *  distribute, sublicense, and/or sell copies of the Software, and to
 *  permit persons to whom the Software is furnished to do so, subject to
 *  the following conditions:
 *
 *  The above copyright notice and this permission notice shall be
 *  included in all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 *  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 *  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 *  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 *  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 *  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 *  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */

#include <gecode/int.hh>

using namespace Gecode;
using namespace Gecode::Int;

// The no-overlap propagator
class NoOverlap : public Propagator {
protected:
    // The x-coordinates
    ViewArray<IntView> x;
    // The width (array)
    int* w;
    // The y-coordinates
    ViewArray<IntView> y;
    // The heights (array)
    int* h;
public:
    // Create propagator and initialize
    NoOverlap(Home home,
              ViewArray<IntView>& x0, int w0[],
              ViewArray<IntView>& y0, int h0[])
    : Propagator(home), x(x0), w(w0), y(y0), h(h0) {
        x.subscribe(home,*this,PC_INT_BND);
        y.subscribe(home,*this,PC_INT_BND);
    }
    // Post no-overlap propagator
    static ExecStatus post(Home home,
                           ViewArray<IntView>& x, int w[],
                           ViewArray<IntView>& y, int h[]) {
        // Only if there is something to propagate
        if (x.size() > 1)
            (void) new (home) NoOverlap(home,x,w,y,h);
        return ES_OK;
    }
    
    // Copy constructor during cloning
    NoOverlap(Space& home, bool share, NoOverlap& p)
    : Propagator(home,share,p) {
        x.update(home,share,p.x);
        y.update(home,share,p.y);
        // Also copy width and height arrays
        w = home.alloc<int>(x.size());
        h = home.alloc<int>(y.size());
        for (int i=x.size(); i--; ) {
            w[i]=p.w[i]; h[i]=p.h[i];
        }
    }
    // Create copy during cloning
    virtual Propagator* copy(Space& home, bool share) {
        return new (home) NoOverlap(home,share,*this);
    }
    
    // Return cost (defined as cheap quadratic)
    virtual PropCost cost(const Space&, const ModEventDelta&) const {
        return PropCost::quadratic(PropCost::LO,2*x.size());
    }
    
    //////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////////
    
    // Perform propagation
    virtual ExecStatus propagate(Space& home, const ModEventDelta&) {
        bool areAllAssigned = true;
        
        // Loop through all squares.
        for (int i = 0; i < x.size(); i++) {
            //Find an assigned square.
            if (x[i].assigned() && y[i].assigned()) {
                for (int j = i + 1; j < x.size(); j++) {
                    // Try to enforce values of square j to not overlap.
                    if (me_failed(x[j].gq(home, x[i].val() + w[i])) &&
                        me_failed(x[j].lq(home, x[i].val() - w[j])) &&
                        me_failed(y[j].gq(home, y[i].val() + h[i])) &&
                        me_failed(y[j].lq(home, y[i].val() - h[j]))) {
                        // If all attempts at enforcing values failed squares are overlapping, report failure.
                        return ES_FAILED;
                    }
                }
            }
            // If there exists an unassigned square set the flag to false so we dont report subsumption.
            else {
                areAllAssigned = false;
            }
        }
        
        // If all suqares were assigned report subsumption.
        if (areAllAssigned) {
            return home.ES_SUBSUMED(*this);
        }
        
        // After executing propagator and not reporting failure or subsumption we are at a fixpoint.
        return ES_FIX;
    }
    
    //////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////////////////////
    
    // Dispose propagator and return its size
    virtual size_t dispose(Space& home) {
        x.cancel(home,*this,PC_INT_BND);
        y.cancel(home,*this,PC_INT_BND);
        (void) Propagator::dispose(home);
        return sizeof(*this);
    }
};

/*
 * Post the constraint that the rectangles defined by the coordinates
 * x and y and width w and height h do not overlap.
 *
 * This is the function that you will call from your model. The best
 * is to paste the entire file into your model.
 */
void nooverlap(Home home,
               const IntVarArgs& x, const IntArgs& w,
               const IntVarArgs& y, const IntArgs& h) {
    // Check whether the arguments make sense
    if ((x.size() != y.size()) || (x.size() != w.size()) ||
        (y.size() != h.size()))
        throw ArgumentSizeMismatch("nooverlap");
    // Never post a propagator in a failed space
    if (home.failed()) return;
    // Set up array of views for the coordinates
    ViewArray<IntView> vx(home,x);
    ViewArray<IntView> vy(home,y);
    // Set up arrays (allocated in home) for width and height and initialize
    int* wc = static_cast<Space&>(home).alloc<int>(x.size());
    int* hc = static_cast<Space&>(home).alloc<int>(y.size());
    for (int i=x.size(); i--; ) {
        wc[i]=w[i]; hc[i]=h[i];
    }
    // If posting failed, fail space
    if (NoOverlap::post(home,vx,wc,vy,hc) != ES_OK)
        home.fail();
}
