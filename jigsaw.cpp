#include <stdarg.h>
#include <assert.h>
#include <string.h>
#include <string>
#include <sstream>
#include <mutex>
#include <thread>
#include <functional>
#include <map>
#include <set>
#include <list>
#include <vector>
#include <bitset>
#include <unordered_set>
#include <unordered_map>

//#define DEBUG
using namespace std;

std::vector<std::string> SplitString(const std::string& s, char delim = ' ', bool reserve_empty_token = false) {
    size_t sz = s.size();
    if (0 == sz || '\0' == delim) return {s};

    std::vector<std::string> tokens;
    size_t p = std::string::npos, lp = 0; // pos and last_pos
    while (lp < sz && std::string::npos != (p = s.find(delim, lp))) {
        if (reserve_empty_token || p > lp) tokens.emplace_back(s.substr(lp, p - lp));
        lp = p + 1;
    }
    if (lp < sz) tokens.emplace_back(s.substr(lp));
    else if (reserve_empty_token) tokens.emplace_back(""); // " a" will return two parts, so " a " should return three parts
    return tokens;
}

std::string string_printf(const char* format, ...) {
    std::string output;
    output.reserve(std::max(32UL, strlen(format) * 2));
    const int write_point = output.size();
    int remaining = output.capacity() - write_point;
    output.resize(output.capacity());

    va_list ap, copied_ap;
    va_start(ap, format);

    va_copy(copied_ap, ap);
    int bytes_used = vsnprintf(&output[write_point], remaining, format, copied_ap);
    va_end(copied_ap);

    if (bytes_used < 0) return "";
    else if (bytes_used < remaining) output.resize(write_point + bytes_used);
    else {
        output.resize(write_point + bytes_used + 1);
        remaining = bytes_used + 1;
        bytes_used = vsnprintf(&output[write_point], remaining, format, ap);
        if (bytes_used + 1 != remaining) return "";
        output.resize(write_point + bytes_used);
    }

    va_end(ap);

    return output;
}

enum BlockType {
    BT_NONE = 0,
    BT_SQUARE = 1,

    //half square, use the position of perpendicular corner to identify them
    BT_LEFT_UP = 2,
    BT_LEFT_DOWN = 3,
    BT_RIGHT_UP = 4,
    BT_RIGHT_DOWN = 5,

    BT_MAX = 6,
};

inline BlockType operator~(BlockType bt) {
    switch (bt) {
    case BT_NONE: return BT_SQUARE;
    case BT_SQUARE: return BT_NONE;
    case BT_RIGHT_DOWN: return BT_LEFT_UP;
    case BT_RIGHT_UP: return BT_LEFT_DOWN;
    case BT_LEFT_UP: return BT_RIGHT_DOWN;
    case BT_LEFT_DOWN: return BT_RIGHT_UP;
    default: break;
    }
    return BT_NONE;
}

enum BlockProvidedType {
    BPT_BAD = 0,
    BPT_S_EMBEDED = 1,
    BPT_TRI_EMBDED = 2,
    BPT_S2_EMBEDED = 3, //might offer s_ru_offers or s_lr_embeded_offers
    BPT_TRI_NORMAL = 4,
    BPT_S_NORMAL = 5,
    BPT_NONE = 6,
};

#define SQUARE_SIZE 2
#define TRIANGLE_SIZE 1

enum RotateType {
    RT_NONE = 0,

    RT_CW_90 = 1,
    RT_CW_180 = 2,
    RT_CW_270 = 3,

    RT_H_FLIP = 4,
    RT_H_FLIP_CW_90 = 5,  //eqaul to: CW_270_H_FLIP
    RT_H_FLIP_CW_180 = 6, //equal to: CW_180_H_FLIP
    RT_H_FLIP_CW_270 = 7, //equal to: CW_90_H_FLIP

    RT_MAX = 8,
};

static int g_rotate_lookup_table[BT_MAX][RT_MAX];

//call this function before main()
__attribute__ ((constructor)) void init_rotate_lookup_table() {
    g_rotate_lookup_table[BT_LEFT_UP][RT_NONE] = BT_LEFT_UP;
    g_rotate_lookup_table[BT_LEFT_DOWN][RT_NONE] = BT_LEFT_DOWN;
    g_rotate_lookup_table[BT_RIGHT_UP][RT_NONE] = BT_RIGHT_UP;
    g_rotate_lookup_table[BT_RIGHT_DOWN][RT_NONE] = BT_RIGHT_DOWN;

    g_rotate_lookup_table[BT_LEFT_UP][RT_CW_90] = BT_RIGHT_UP;
    g_rotate_lookup_table[BT_LEFT_DOWN][RT_CW_90] = BT_LEFT_UP;
    g_rotate_lookup_table[BT_RIGHT_UP][RT_CW_90] = BT_RIGHT_DOWN;
    g_rotate_lookup_table[BT_RIGHT_DOWN][RT_CW_90] = BT_LEFT_DOWN;

    g_rotate_lookup_table[BT_LEFT_UP][RT_CW_180] = BT_RIGHT_DOWN;
    g_rotate_lookup_table[BT_LEFT_DOWN][RT_CW_180] = BT_RIGHT_UP;
    g_rotate_lookup_table[BT_RIGHT_UP][RT_CW_180] = BT_LEFT_DOWN;
    g_rotate_lookup_table[BT_RIGHT_DOWN][RT_CW_180] = BT_LEFT_UP;

    g_rotate_lookup_table[BT_LEFT_UP][RT_CW_270] = BT_LEFT_DOWN;
    g_rotate_lookup_table[BT_LEFT_DOWN][RT_CW_270] = BT_RIGHT_DOWN;
    g_rotate_lookup_table[BT_RIGHT_UP][RT_CW_270] = BT_LEFT_UP;
    g_rotate_lookup_table[BT_RIGHT_DOWN][RT_CW_270] = BT_RIGHT_UP;

    g_rotate_lookup_table[BT_LEFT_UP][RT_H_FLIP] = BT_RIGHT_UP;
    g_rotate_lookup_table[BT_LEFT_DOWN][RT_H_FLIP] = BT_RIGHT_DOWN;
    g_rotate_lookup_table[BT_RIGHT_UP][RT_H_FLIP] = BT_LEFT_UP;
    g_rotate_lookup_table[BT_RIGHT_DOWN][RT_H_FLIP] = BT_LEFT_DOWN;

    g_rotate_lookup_table[BT_LEFT_UP][RT_H_FLIP_CW_90] = BT_RIGHT_DOWN;
    g_rotate_lookup_table[BT_LEFT_DOWN][RT_H_FLIP_CW_90] = BT_LEFT_DOWN;
    g_rotate_lookup_table[BT_RIGHT_UP][RT_H_FLIP_CW_90] = BT_RIGHT_UP;
    g_rotate_lookup_table[BT_RIGHT_DOWN][RT_H_FLIP_CW_90] = BT_LEFT_UP;

    g_rotate_lookup_table[BT_LEFT_UP][RT_H_FLIP_CW_180] = BT_LEFT_DOWN;
    g_rotate_lookup_table[BT_LEFT_DOWN][RT_H_FLIP_CW_180] = BT_LEFT_UP;
    g_rotate_lookup_table[BT_RIGHT_UP][RT_H_FLIP_CW_180] = BT_RIGHT_DOWN;
    g_rotate_lookup_table[BT_RIGHT_DOWN][RT_H_FLIP_CW_180] = BT_RIGHT_UP;

    g_rotate_lookup_table[BT_LEFT_UP][RT_H_FLIP_CW_270] = BT_LEFT_UP;
    g_rotate_lookup_table[BT_LEFT_DOWN][RT_H_FLIP_CW_270] = BT_RIGHT_UP;
    g_rotate_lookup_table[BT_RIGHT_UP][RT_H_FLIP_CW_270] = BT_LEFT_DOWN;
    g_rotate_lookup_table[BT_RIGHT_DOWN][RT_H_FLIP_CW_270] = BT_RIGHT_DOWN;
}

struct Block {
    BlockType bt = BT_NONE;
    int down = -1;
    int right = -1;
    int up = -1;
    int left = -1;

    int rel_r = 0; //row number relative to the position of blks[0]
    int rel_c = 0; //column number relative to the position of blks[0]

    string GetDebugString() const {
        return string_printf("(bt:%d, down:%d, up:%d, right:%d, left:%d)", bt, down, up, right, left);
    }
};

//https://www.codeproject.com/Articles/5329247/How-to-Change-Text-Color-in-a-Linux-Terminal
#define RED     "\033[31m"
#define GREEN   "\033[32m"
#define YELLOW  "\033[33m"
#define BLUE    "\033[34m"
#define MAGENTA "\033[35m"
#define CYAN    "\033[36m"
#define WHITE   "\033[97m"

#define BRIGHT_RED     "\033[31;1m"
#define BRIGHT_GREEN   "\033[32;1m"
#define BRIGHT_YELLOW  "\033[33;1m"
#define BRIGHT_BLUE    "\033[34;1m"
#define BRIGHT_MAGENTA "\033[35;1m"
#define BRIGHT_CYAN    "\033[36;1m"
#define BRIGHT_WHITE   "\033[97;1m"

#define END     "\033[0m"
#define INVALID "\033[50m"

enum ColorType {
    CT_NONE = 0,

    CT_RED     = 1,
    CT_GREEN   = 2,
    CT_YELLOW  = 3,
    CT_BLUE    = 4,
    CT_MAGENTA = 5,
    CT_CYAN    = 6,
    CT_WHITE   = 7,

    CT_BRIGHT_RED     = CT_RED     + 7,
    CT_BRIGHT_GREEN   = CT_GREEN   + 7,
    CT_BRIGHT_YELLOW  = CT_YELLOW  + 7,
    CT_BRIGHT_BLUE    = CT_BLUE    + 7,
    CT_BRIGHT_MAGENTA = CT_MAGENTA + 7,
    CT_BRIGHT_CYAN    = CT_CYAN    + 7,
    CT_BRIGHT_WHITE   = CT_WHITE   + 7,

    CT_MAX = 15,
    CT_INVALID = 16,
};

string ColorText(const string& s, ColorType ct) {
    switch (ct) {
    case CT_RED: return RED + s + END;
    case CT_GREEN: return GREEN + s + END;
    case CT_YELLOW: return YELLOW + s + END;
    case CT_BLUE: return BLUE + s + END;
    case CT_MAGENTA: return MAGENTA + s + END;
    case CT_CYAN: return CYAN + s + END;
    case CT_WHITE: return WHITE + s + END;
    case CT_BRIGHT_RED: return BRIGHT_RED + s + END;
    case CT_BRIGHT_GREEN: return BRIGHT_GREEN + s + END;
    case CT_BRIGHT_YELLOW: return BRIGHT_YELLOW + s + END;
    case CT_BRIGHT_BLUE: return BRIGHT_BLUE + s + END;
    case CT_BRIGHT_MAGENTA: return BRIGHT_MAGENTA + s + END;
    case CT_BRIGHT_CYAN: return BRIGHT_CYAN + s + END;
    case CT_BRIGHT_WHITE: return BRIGHT_WHITE + s + END;
    case CT_INVALID: return INVALID + s + END;
    default: break;
    }
    return s;
}

enum DirectionType {
    DT_NONE = 0,

    DT_RIGHT = 1,
    DT_DOWN = 2,
    DT_LEFT = 3,
    DT_UP = 4,

    DT_MAX = 5,
};

struct WallsInfo {
    /*
    WallsInfo(bool u, bool d, bool l, bool r) {
        walls_info[DT_UP] = u;
        walls_info[DT_DOWN] = d;
        walls_info[DT_LEFT] = l;
        walls_info[DT_RIGHT] = f;
    }
    */

    bool operator[](DirectionType dt) const {
        return walls_info[dt];
    }
    void Set(DirectionType dt, bool is_wall) {
        walls_info[dt] = is_wall;
    }
    int GetWallsCount() const {
        return walls_info.count();
    }
    bitset<DT_MAX> walls_info;
};

struct Shape {
    Shape() {}

    //usage: refer to Model::Model(const string& steps)
    Shape(const string& steps) {
        function<void(const string&, int)> helper_fn;
        helper_fn = [&](const string& route, int prev_blk_id) {
            BlockType bt = BT_NONE;
            DirectionType dt = DT_NONE;
            for (int i = 0; i < (int)route.size(); ++i) {
                if (' ' == route[i]) continue;
                if ('s' == route[i]) {
                    bt = BT_SQUARE;
                } else if ('r' == route[i]) {
                    if ('u' == route[i + 1]) {
                        bt = BT_RIGHT_UP;
                    } else if ('d' == route[i + 1]) {
                        bt = BT_RIGHT_DOWN;
                    }
                    ++i;
                } else if ('l' == route[i]) {
                    if ('u' == route[i + 1]) {
                        bt = BT_LEFT_UP;
                    } else if ('d' == route[i + 1]) {
                        bt = BT_LEFT_DOWN;
                    }
                    ++i;
                } else {
                    if (BT_NONE != bt) prev_blk_id = AddBlock(bt, prev_blk_id, dt);

                    if ('<' == route[i]) {
                        dt = DT_LEFT;
                    } else if ('>' == route[i]) {
                        dt = DT_RIGHT;
                    } else if ('^' == route[i]) {
                        dt = DT_UP;
                    } else if ('v' == route[i]) {
                        dt = DT_DOWN;
                    } else if ('(' == route[i]) {
                        assert(')' == route.back());
                        assert(string::npos == route.find('(', i + 1)); // recursive nesting is not supported
                        const auto& sub_routes = SplitString(route.substr(i + 1, route.size() - i - 2), '|');
                        for (auto& sr : sub_routes) helper_fn(sr, prev_blk_id);
                        return;
                    }
                }
            }
            if (DT_NONE != dt || BT_NONE != bt) {
                prev_blk_id = AddBlock(bt, prev_blk_id, dt);
            }
        };

        helper_fn(steps, -1);
        BuildOver();
    }

    //return the block id that is added
    int AddBlock(BlockType bt, int prev_blk_id = -1, DirectionType dt = DT_NONE) {
        auto sz = blks.size();
        blks.resize(sz + 1);
        blks[sz].bt = bt;
        if (prev_blk_id < 0) return sz;

        switch (dt) {
        case DT_RIGHT: blks[prev_blk_id].right = sz; blks[sz].left = prev_blk_id; break;
        case DT_DOWN: blks[prev_blk_id].down = sz; blks[sz].up = prev_blk_id; break;
        case DT_LEFT: blks[prev_blk_id].left = sz; blks[sz].right = prev_blk_id; break;
        case DT_UP: blks[prev_blk_id].up = sz; blks[sz].down = prev_blk_id; break;
        default: break;
        }
        return sz;
    }

    string GetDebugString() const {
        ostringstream oss;
        for (int i = 0; i < (int)blks.size(); ++i) {
            oss << "{[" << i << "]" << blks[i].GetDebugString() << "}";
        }
        return oss.str();
    }

    int size() const {
        return blks.size();
    }
    void resize(int n) {
        blks.resize(n);
    }
    const Block& operator[](int idx) const {
        return blks[idx];
    }
    Block& operator[](int idx) {
        return blks[idx];
    }

    bool operator==(const Shape& s2) const;
    void Print() const;

    //get the shape itself blocks and the outline blocks that are close to shape self, using the (rel_r, rel_c) to blk0 to locate each block
    const set<pair<int, int>>& GetBlocksWithOutline() const {
        return blks_with_outlines;
    }

    //assume blks[0] at (0, 0)
    void BuildOver() {
        //use 'shape_area_size' as a flag showing whether the build process is over
        if (shape_area_size > 0) return;

        int rm = 0, rn = 0;
        int cm = 0, cn = 0;
        set<int> visited;
        map<pair<int, int>, int> uniq_blks_pos;
        std::function<void(int, int, int)> fn;
        fn = [&](int idx, int r, int c) {
            visited.insert(idx);
            rm = max(rm, r); rn = min(rn, r);
            cm = max(cm, c); cn = min(cn, c);

            blks_with_outlines.insert({r, c});
            blks_with_outlines.insert({r - 1, c});
            blks_with_outlines.insert({r + 1, c});
            blks_with_outlines.insert({r, c - 1});
            blks_with_outlines.insert({r, c + 1});
            auto& b = (*this)[idx];
            if (BT_SQUARE == b.bt) {
                shape_area_size += SQUARE_SIZE;
            } else if (BT_NONE != b.bt) {
                shape_area_size += TRIANGLE_SIZE;
            }
            b.rel_r = r;
            b.rel_c = c;
            assert(uniq_blks_pos.insert({{r, c}, idx}).second);
            if (b.up < 0) {
                if (uniq_blks_pos.end() != uniq_blks_pos.find({r - 1, c})) {
                    int idx_up = uniq_blks_pos[{r - 1, c}];
                    BlockType bt_up = (*this)[idx_up].bt;
                    if (BT_SQUARE == bt_up || BT_LEFT_DOWN == bt_up || BT_RIGHT_DOWN == bt_up) {
                        b.up = idx_up;
                        (*this)[idx_up].down = idx;
                    }
                }
            } else if (visited.find(b.up) == visited.end()) fn(b.up, r - 1, c);

            if (b.down < 0) {
                if (uniq_blks_pos.end() != uniq_blks_pos.find({r + 1, c})) {
                    int idx_down = uniq_blks_pos[{r + 1, c}];
                    BlockType bt_down = (*this)[idx_down].bt;
                    if (BT_SQUARE == bt_down || BT_LEFT_UP == bt_down || BT_RIGHT_UP == bt_down) {
                        b.down = idx_down;
                        (*this)[idx_down].up = idx;
                    }
                }
            } else if (visited.find(b.down) == visited.end()) fn(b.down, r + 1, c);

            if (b.left < 0) {
                if (uniq_blks_pos.end() != uniq_blks_pos.find({r, c - 1})) {
                    int idx_left = uniq_blks_pos[{r, c - 1}];
                    BlockType bt_left = (*this)[idx_left].bt;
                    if (BT_SQUARE == bt_left || BT_RIGHT_UP == bt_left || BT_RIGHT_DOWN == bt_left) {
                        b.left = idx_left;
                        (*this)[idx_left].right = idx;
                    }
                }
            } else if (visited.find(b.left) == visited.end()) fn(b.left, r, c - 1);

            if (b.right < 0) {
                if (uniq_blks_pos.end() != uniq_blks_pos.find({r, c + 1})) {
                    int idx_right = uniq_blks_pos[{r, c + 1}];
                    BlockType bt_right = (*this)[idx_right].bt;
                    if (BT_SQUARE == bt_right || BT_LEFT_UP == bt_right || BT_LEFT_DOWN == bt_right) {
                        b.right = idx_right;
                        (*this)[idx_right].left = idx;
                    }
                }
            } else if (visited.find(b.right) == visited.end()) fn(b.right, r , c + 1);
        };

        fn(0, 0, 0);

        rows = rm - rn + 1;
        cols = cm - cn + 1;
        blk0_offset_r = -rn;
        blk0_offset_c = -cn;
        assert(shape_area_size > TRIANGLE_SIZE);

        for (auto& b : blks) {
            int sur_blk_cnt = (b.up >= 0) + (b.down >= 0) + (b.left >= 0) + (b.right >= 0);
            if (BT_SQUARE == b.bt) {
                if (1 == sur_blk_cnt) {
                    ++s_embeded_blk_cnt;
                } else if (2 == sur_blk_cnt) ++s2_embeded_blk_cnt;
            } else if (1 == sur_blk_cnt) ++tri_embeded_blk_cnt;
        }

        //calculate hash value in advance
        string str(rows * cols * 5, 0);
        Block tmp_blk, *p_blk = nullptr;
        for (int r = rn, idx = 0; r <= rm; ++r) {
            for (int c = cn; c <= cm; ++c) {
                p_blk = &tmp_blk;
                auto iter = uniq_blks_pos.find({r, c});
                if (uniq_blks_pos.end() != iter) {
                    p_blk = &blks[iter->second];
                }
                str[idx++] = p_blk->bt;
                str[idx++] = p_blk->left >= 0;
                str[idx++] = p_blk->right >= 0;
                str[idx++] = p_blk->up >= 0;
                str[idx++] = p_blk->down >= 0;
            }
        }
        //use two hash values to achieve the hash conflict close to 0 for optimization
        hash_val = hash<string>()(string_printf("%dx%d", rows, cols) + str);
        hash_val2 = hash<string>()(str + string_printf("%dx%d", rows, cols));
    }

    //check the wall status(true/false) for the current block from right/left/up/down directions:
    //result[DT_NONE]; //ignore
    //result[DT_RIGHT];
    //result[DT_DOWN];
    //result[DT_LEFT];
    //result[DT_UP];
    WallsInfo AnalyseWallsAroundBlock(int blk_idx) const;

    //return (rows, columns)
    pair<int, int> GetCircumRectangleSize() const {
        return {rows, cols};
    }

    inline int GetShapeAreaSize() const {
        return shape_area_size;
    }

    //if you put this block in the upper left corner(0, 0), get the offset of its starting block(blks[0])
    pair<int, int> GetStartBlockOffset() const {
        return {blk0_offset_r, blk0_offset_c};
    }

    vector<Block> blks;
    set<pair<int, int>> blks_with_outlines;
    int blk0_offset_r = 0;
    int blk0_offset_c = 0;
    int rows = 0;
    int cols = 0;
    uint16_t shape_area_size = 0;
    uint16_t s_embeded_blk_cnt = 0;
    uint16_t tri_embeded_blk_cnt = 0;
    uint16_t s2_embeded_blk_cnt = 0;
    size_t hash_val = 0;
    size_t hash_val2 = 0;
};

template <>
struct std::hash<Shape> {
    std::size_t operator()(const Shape& s) const {
        assert(0 != s.hash_val);
        return s.hash_val;
    }
};

struct Model {
    Model() {
        shapes.resize(8);
    }

    Model(const Shape& s) {
        shapes.push_back(s);
        shapes.resize(8);
        BuildOver();
    }

    //construct a shape within one stroke as far as possible, except those like 'cross' shapes:
    //shape: "s|lu|ld|ru|rd"
    //direction: "^|v|<|>"
    //color: ";1-14" //at the end
    //fork: "s>s(>s|vs)" //'T' shape
    //
    //examples:
    //    "svs>ld;1"
    //    "s>s(^s|vs|>s);12"
    Model(const string& steps) {
        size_t p = steps.find(';');
        new (this) Model(Shape(steps.substr(0, p)));
        if (string::npos != p) {
            model_ct = (ColorType)atoi(&steps[p + 1]); //must be at the end
        }
    }

    void BuildOver() {
        offers.resize(BT_MAX);
        auto rotate_clockwise_90 = [&](const Shape&s, Shape& s2) {
            s2.resize(s.size());
            for (int i = 0; i < (int)s.size(); ++i) {
                auto& b = s[i];
                auto& b2 = s2[i];
                if (b.right >= 0) {
                    b2.down = b.right;
                    s2[b.right].up = i;
                }
                if (b.up >= 0) {
                    b2.right = b.up;
                    s2[b.up].left = i;
                }
                if (b.left >= 0) {
                    b2.up = b.left;
                    s2[b.left].down = i;
                }
                if (b.down >= 0) {
                    b2.left = b.down;
                    s2[b.down].right = i;
                }
                switch (b.bt) {
                case BT_SQUARE: b2.bt = BT_SQUARE; break;
                case BT_RIGHT_DOWN: b2.bt = BT_LEFT_DOWN; break;
                case BT_LEFT_DOWN: b2.bt = BT_LEFT_UP; break;
                case BT_RIGHT_UP: b2.bt = BT_RIGHT_DOWN; break;
                case BT_LEFT_UP: b2.bt = BT_RIGHT_UP; break;
                default: break;
                }
            }
        };
        rotate_clockwise_90(shapes[0], shapes[1]);
        rotate_clockwise_90(shapes[1], shapes[2]);
        rotate_clockwise_90(shapes[2], shapes[3]);
        { //flip horizontally
            const auto& s = shapes[0];
            auto& s2 = shapes[4];
            s2.resize(s.size());
            for (int i = 0; i < (int)s.size(); ++i) {
                auto& b = s[i];
                auto& b2 = s2[i];
                if (b.right >= 0) {
                    b2.left = b.right;
                    s2[b.right].right = i;
                }
                if (b.left >= 0) {
                    b2.right = b.left;
                    s2[b.left].left = i;
                }
                if (b.up >= 0) {
                    b2.up = b.up;
                    s2[b.up].down = i;
                }
                if (b.down >= 0) {
                    b2.down = b.down;
                    s2[b.down].up = i;
                }
                switch (b.bt) {
                case BT_SQUARE: b2.bt = BT_SQUARE; break;
                case BT_RIGHT_DOWN: b2.bt = BT_LEFT_DOWN; break;
                case BT_LEFT_DOWN: b2.bt = BT_RIGHT_DOWN; break;
                case BT_RIGHT_UP: b2.bt = BT_LEFT_UP; break;
                case BT_LEFT_UP: b2.bt = BT_RIGHT_UP; break;
                default: break;
                }
            }
        };
        rotate_clockwise_90(shapes[4], shapes[5]);
        rotate_clockwise_90(shapes[5], shapes[6]);
        rotate_clockwise_90(shapes[6], shapes[7]);

        for (auto& s : shapes) s.BuildOver();

        //dedup shapes
        int uniq_idx = 1;
        for (int i = 1; i < (int)shapes.size(); ++i) {
            bool uniq = true;
            for (int j = 0; j < i; ++j) {
                if (shapes[i] == shapes[j]) {
                    uniq = false;
                    break;
                }
            }
            if (!uniq) continue;
            if (uniq_idx != i) swap(shapes[uniq_idx], shapes[i]);
            ++uniq_idx;
        }
        shapes.resize(uniq_idx);

        for (auto& s : shapes) hash_val += hash<Shape>()(s);

        for (int i = 0; i < (int)shapes.size(); ++i) { //shape idx
            auto& s = shapes[i];
            for (int j = 0; j < (int)s.size(); ++j) { //block idx
                auto& b = s[j];
                const auto& walls = s.AnalyseWallsAroundBlock(j);
                int walls_cnt = walls[DT_RIGHT] + walls[DT_LEFT] + walls[DT_UP] + walls[DT_DOWN];
                switch (b.bt) {
                case BT_SQUARE:
                    if (b.right < 0 || b.left < 0 || b.up < 0 || b.down < 0) offers[b.bt].insert({i, j});
                    if (walls_cnt <= 1) {
                        if (walls[DT_RIGHT]) {
                            s_r_embeded_offers.insert({i, j});
                            s_lr_embeded_offers.insert({i, j});
                            s_ru_offers.insert({i, j});
                            s_rd_offers.insert({i, j});
                        }
                        if (walls[DT_LEFT]) {
                            s_l_embeded_offers.insert({i, j});
                            s_lr_embeded_offers.insert({i, j});
                            s_lu_offers.insert({i, j});
                            s_ld_offers.insert({i, j});
                        }
                        if (walls[DT_UP]) {
                            s_u_embeded_offers.insert({i, j});
                            s_ud_embeded_offers.insert({i, j});
                            s_lu_offers.insert({i, j});
                            s_ru_offers.insert({i, j});
                        }
                        if (walls[DT_DOWN]) {
                            s_d_embeded_offers.insert({i, j});
                            s_ud_embeded_offers.insert({i, j});
                            s_ld_offers.insert({i, j});
                            s_rd_offers.insert({i, j});
                        }
                    } else if (2 == walls_cnt) {
                        if (walls[DT_RIGHT] && walls[DT_UP]) s_ru_offers.insert({i, j});
                        else if (walls[DT_LEFT] && walls[DT_UP]) s_lu_offers.insert({i, j});
                        else if (walls[DT_RIGHT] && walls[DT_DOWN]) s_rd_offers.insert({i, j});
                        else if (walls[DT_LEFT] && walls[DT_DOWN]) s_ld_offers.insert({i, j});
                        else if (walls[DT_LEFT] && walls[DT_RIGHT]) s_lr_embeded_offers.insert({i, j});
                        else if (walls[DT_UP] && walls[DT_DOWN]) s_ud_embeded_offers.insert({i, j});
                    }
                    break;
                case BT_RIGHT_DOWN:
                    if (walls_cnt <= 1) {
                        if (walls[DT_RIGHT]) {
                            rd_r_embeded_offers.insert({i, j});
                        }
                        if (walls[DT_DOWN]) {
                            rd_d_embeded_offers.insert({i, j});
                        }
                    }
                    offers[b.bt].insert({i, j});
                    break;
                case BT_LEFT_DOWN:
                    if (walls_cnt <= 1) {
                        if (walls[DT_LEFT]) {
                            ld_l_embeded_offers.insert({i, j});
                        }
                        if (walls[DT_DOWN]) {
                            ld_d_embeded_offers.insert({i, j});
                        }
                    }
                    offers[b.bt].insert({i, j});
                    break;
                case BT_RIGHT_UP:
                    if (walls_cnt <= 1) {
                        if (walls[DT_RIGHT]) {
                            ru_r_embeded_offers.insert({i, j});
                        }
                        if (walls[DT_UP]) {
                            ru_u_embeded_offers.insert({i, j});
                        }
                    }
                    offers[b.bt].insert({i, j});
                    break;
                case BT_LEFT_UP:
                    if (walls_cnt <= 1) {
                        if (walls[DT_LEFT]) {
                            lu_l_embeded_offers.insert({i, j});
                        }
                        if (walls[DT_UP]) {
                            lu_u_embeded_offers.insert({i, j});
                        }
                    }
                    offers[b.bt].insert({i, j});
                    break;
                default:break;
                }
            }
        }
    }

    bool operator==(const Model& other) const;

    const vector<Shape>& GetShapes() const {
        return shapes;
    }

    inline int GetModelAreaSize() const {
        assert(!shapes.empty());
        return shapes[0].GetShapeAreaSize();
    }

    ColorType GetModelColor() const {
        return model_ct;
    }

    void SetModelColor(ColorType ct) {
        model_ct = ct;
    }

    void Print() const;

    vector<Shape> shapes;
    ColorType model_ct = CT_NONE;
    size_t hash_val = 0;

    //fit level: BPT_TRI_NORMAL or BPT_S_NORMAL(easy)
    vector<set<pair<int, int>>> offers;

    //fit level: BPT_S2_EMBEDED(normal)
    set<pair<int, int>> s_rd_offers; //match square from rd direction
    set<pair<int, int>> s_ru_offers; //match square from ru direction
    set<pair<int, int>> s_ld_offers; //match square from ld direction
    set<pair<int, int>> s_lu_offers; //match square from lu direction
    set<pair<int, int>> s_ud_embeded_offers; //match embeded square from u or d direction
    set<pair<int, int>> s_lr_embeded_offers; //match embeded square from l or r direction

    //fit level: BPT_TRI_EMBDED(hard)
    set<pair<int, int>> lu_l_embeded_offers; //match embeded lu triangle from l direction
    set<pair<int, int>> lu_u_embeded_offers; //match embeded lu triangle from u direction
    set<pair<int, int>> ld_l_embeded_offers; //match embeded ld triangle from l direction
    set<pair<int, int>> ld_d_embeded_offers; //match embeded ld triangle from d direction
    set<pair<int, int>> ru_r_embeded_offers; //match embeded ru triangle from r direction
    set<pair<int, int>> ru_u_embeded_offers; //match embeded ru triangle from u direction
    set<pair<int, int>> rd_r_embeded_offers; //match embeded rd triangle from r direction
    set<pair<int, int>> rd_d_embeded_offers; //match embeded rd triangle from d direction

    //fit level: BPT_S_EMBEDED(very hard)
    set<pair<int, int>> s_l_embeded_offers; //match embeded square from l direction
    set<pair<int, int>> s_r_embeded_offers; //match embeded square from r direction
    set<pair<int, int>> s_u_embeded_offers; //match embeded square from u direction
    set<pair<int, int>> s_d_embeded_offers; //match embeded square from d direction
};

template <>
struct std::hash<Model> {
    std::size_t operator()(const Model& m) const {
        assert(0 != m.hash_val);
        return m.hash_val;
    }
};

bool Model::operator==(const Model& m2) const {
    for (auto& s : m2.GetShapes()) {
        if (s == shapes[0]) {
            return true;
        }
    }
    return false;
}

struct Canvas;
void PrintCanvases(const vector<Canvas>& canvases, bool padding = true);

struct Canvas {
    struct Cell {
        static constexpr const char* top_corner = "^";
        static constexpr const char* top_line = "----";
        static constexpr const char* bottom_corner = "v";
        static constexpr const char* bottom_line = "----";

        static constexpr const char* s_middle  = "|    |";
        static constexpr const char* lu_middle = "| /";
        static constexpr const char* rd_middle = "/ |";
        static constexpr const char* ru_middle = "\\ |";
        static constexpr const char* ld_middle = "| \\";

        Cell() {
            InitCellLines();
        }

        void InitCellLines() {
            cell_lines[0] = ""; cell_lines[0].resize(2 * strlen(top_corner) + strlen(top_line), ' ');
            cell_lines[1] = ""; cell_lines[1].resize(strlen(s_middle), ' ');
            cell_lines[2] = ""; cell_lines[2].resize(2 * strlen(bottom_corner) + strlen(bottom_line), ' ');
        }

        // focus their shapes only
        bool operator==(const Cell& c2) const {
            if (!(left_open == c2.left_open && right_open == c2.right_open && up_open == c2.up_open && down_open == c2.down_open)) return false;
            return (bt == c2.bt && bt2 == c2.bt2) || (bt == c2.bt2 && bt2 == c2.bt);
        }

        inline bool CanPaintBlock(BlockType spec_bt) const {
            if ((BT_SQUARE == bt) || (BT_NONE != bt2)) return false;
            if (BT_NONE == spec_bt) return true; //check whether this cell has been fully occupied if BT_NONE == bt
            if (BT_RIGHT_DOWN == bt && BT_LEFT_UP != spec_bt) return false;
            if (BT_LEFT_UP == bt && BT_RIGHT_DOWN != spec_bt) return false;
            if (BT_LEFT_DOWN == bt && BT_RIGHT_UP != spec_bt) return false;
            if (BT_RIGHT_UP == bt && BT_LEFT_DOWN != spec_bt) return false;
            return true;
        }

        inline BlockType GetBlankBlockType() const {
            return blank_bt;
        }

        inline bool EraseBlockMark(BlockType spec_bt) {
            if (BT_NONE == spec_bt) return true;
            if (BT_NONE == bt) return false;

            if (bt == spec_bt) {
                if (BT_NONE != bt2) {
                    bt = bt2;
                    ct = ct2;
                    bt2 = BT_NONE;
                    ct2 = CT_NONE;
                } else {
                    bt = BT_NONE;
                    ct = CT_NONE;
                }
            } else if (bt2 == spec_bt) {
                bt2 = BT_NONE;
                ct2 = CT_NONE;
            } else if (BT_SQUARE == bt && BT_SQUARE != spec_bt) {
                switch (spec_bt) {
                case BT_LEFT_DOWN: bt = BT_RIGHT_UP; break;
                case BT_LEFT_UP: bt = BT_RIGHT_DOWN; break;
                case BT_RIGHT_DOWN: bt = BT_LEFT_UP; break;
                case BT_RIGHT_UP: bt = BT_LEFT_DOWN; break;
                default: break;
                }
            } else {
                return false;
            }
            blank_bt = ~bt;

            switch (spec_bt) {
            case BT_SQUARE: left_open = false, right_open = false, up_open = false, down_open = false; break;
            case BT_LEFT_DOWN: left_open = false, down_open = false; break;
            case BT_LEFT_UP: left_open = false, up_open = false; break;
            case BT_RIGHT_DOWN: right_open = false, down_open = false; break;
            case BT_RIGHT_UP: right_open = false, up_open = false; break;
            default: break;
            }

            return true;
        }

        bool HasPaintedCellLines() const {
            return has_painted_cell_lines;
        }

        void PaintCellLines(BlockType spec_bt = BT_NONE, ColorType spec_ct = CT_NONE) {
            auto s_replace_fn = [](const string& s, const string& delim, const std::string& ns, bool replace_after_delim = true) {
                auto p = s.find(delim);
                assert(string::npos != p);
                if (replace_after_delim) return s.substr(0, p + delim.size()) + ns;
                return ns + s.substr(p);
            };

            auto paint_fn = [&](BlockType bt_param, ColorType ct_param, bool force_paint_bt = false) {
                switch (bt_param) {
                case BT_SQUARE:
                    cell_lines[0] = ColorText(string(top_corner) + top_line + top_corner, ct_param);
                    cell_lines[1] = ColorText(s_middle, ct_param);
                    cell_lines[2] = ColorText(string(bottom_corner) + bottom_line + bottom_corner, ct_param);
                    break;
                case BT_RIGHT_DOWN:
                    if (force_paint_bt || BT_NONE == bt) {
                        cell_lines[0] = cell_lines[0].substr(0, cell_lines[0].size() - strlen(top_corner)) + ColorText(top_corner, ct_param);
                    }
                    if (force_paint_bt || BT_NONE == bt || CT_NONE == ct) {
                        cell_lines[1] = cell_lines[1].substr(0, cell_lines[1].size() - strlen(rd_middle)) + ColorText(rd_middle, ct_param);
                        cell_lines[2] = ColorText(string(bottom_corner) + bottom_line + bottom_corner, ct_param);
                    } else {
                        cell_lines[1] = s_replace_fn(cell_lines[1], END, ColorText(rd_middle, ct_param));
                        cell_lines[2] = s_replace_fn(cell_lines[2], END, ColorText((string)bottom_line + bottom_corner, ct_param));
                    }
                    break;
                case BT_LEFT_DOWN:
                    if (force_paint_bt || BT_NONE == bt) {
                        cell_lines[0] = ColorText(top_corner, ct_param) + cell_lines[0].substr(strlen(top_corner));
                    }
                    if (force_paint_bt || BT_NONE == bt || CT_NONE == ct) {
                        cell_lines[1] = ColorText(ld_middle, ct_param) + cell_lines[1].substr(strlen(ld_middle));
                        cell_lines[2] = ColorText(string(bottom_corner) + bottom_line + bottom_corner, ct_param);
                    } else {
                        cell_lines[1] = s_replace_fn(cell_lines[1], "\033[", ColorText(ld_middle, ct_param), false);
                        cell_lines[2] = s_replace_fn(cell_lines[2], "\033[", ColorText(string(bottom_corner) + bottom_line, ct_param), false);
                    }
                    break;
                case BT_RIGHT_UP:
                    if (force_paint_bt || BT_NONE == bt) {
                        cell_lines[2] = cell_lines[2].substr(0, cell_lines[2].size() - strlen(bottom_corner)) + ColorText(bottom_corner, ct_param);
                    }
                    if (force_paint_bt || BT_NONE == bt || CT_NONE == ct) {
                        cell_lines[0] = ColorText(string(top_corner) + top_line + top_corner, ct_param);
                        cell_lines[1] = cell_lines[1].substr(0, cell_lines[1].size() - strlen(ru_middle)) + ColorText(ru_middle, ct_param);
                    } else {
                        cell_lines[0] = s_replace_fn(cell_lines[0], END, ColorText((string)top_line + top_corner, ct_param));
                        cell_lines[1] = s_replace_fn(cell_lines[1], END, ColorText(ru_middle, ct_param));
                    }
                    break;
                case BT_LEFT_UP:
                    if (force_paint_bt || BT_NONE == bt) {
                        cell_lines[2] = ColorText(bottom_corner, ct_param) + cell_lines[2].substr(strlen(bottom_corner));
                    }
                    if (force_paint_bt || BT_NONE == bt || CT_NONE == ct) {
                        cell_lines[0] = ColorText(string(top_corner) + top_line + top_corner, ct_param);
                        cell_lines[1] = ColorText(lu_middle, ct_param) + cell_lines[1].substr(strlen(lu_middle));
                    } else {
                        cell_lines[0] = s_replace_fn(cell_lines[0], "\033[", ColorText(string(top_corner) + top_line, ct_param), false);
                        cell_lines[1] = s_replace_fn(cell_lines[1], "\033[", ColorText(lu_middle, ct_param), false);
                    }
                    break;
                default:
                    break;
                }
            };

            if (BT_NONE == spec_bt || CT_NONE == spec_ct) {
                InitCellLines();
                paint_fn(bt, ct, true);
                paint_fn(bt2, ct2);
            } else {
                paint_fn(spec_bt, spec_ct);
            }
            has_painted_cell_lines = true;
        }

        bool PaintBlock(BlockType spec_bt, ColorType spec_ct, bool just_mark = false) {
            if (!CanPaintBlock(spec_bt)) return false;

            if (!just_mark) {
                PaintCellLines(spec_bt, spec_ct);
            }

            if (BT_NONE == bt) {
                bt = spec_bt;
                ct = spec_ct;
                blank_bt = ~bt;
            } else {
                bt2 = spec_bt;
                ct2 = spec_ct;
                blank_bt = BT_NONE;
            }

            return true;
        }

        void Clean() {
            *this = Cell();
        }

        void Rotate(RotateType rt, bool repaint = false) {
            auto rotate_wall_open_clockwise_90_fn = [this]() {
                bool tmp = left_open;
                left_open = down_open;
                down_open = right_open;
                right_open = up_open;
                up_open = tmp;
            };
            auto rotate_wall_open_h_flip_fn = [this]() {
                bool tmp = left_open;
                left_open = right_open;
                right_open = tmp;
            };

            switch (rt) {
            case RT_CW_270:
                rotate_wall_open_clockwise_90_fn();
            case RT_CW_180:
                rotate_wall_open_clockwise_90_fn();
            case RT_CW_90:
                rotate_wall_open_clockwise_90_fn();
                break;
            case RT_H_FLIP_CW_90:
                rotate_wall_open_clockwise_90_fn();
            case RT_H_FLIP_CW_180:
                rotate_wall_open_clockwise_90_fn();
            case RT_H_FLIP_CW_270:
                rotate_wall_open_clockwise_90_fn();
            case RT_H_FLIP:
                rotate_wall_open_h_flip_fn();
                break;
            default: break;
            }

            if (BT_SQUARE == bt) return;
            bt = (BlockType)g_rotate_lookup_table[bt][rt];
            if (BT_NONE != bt2) bt2 = (BlockType)g_rotate_lookup_table[bt2][rt];
            else blank_bt = ~bt;

            if (repaint) {
                Cell c2;
                c2.left_open = left_open; c2.right_open = right_open;
                c2.up_open = up_open; c2.down_open = down_open;
                assert(c2.PaintBlock(bt, ct));
                if (BT_NONE != bt2) assert(c2.PaintBlock(bt2, ct2));
                *this = c2;
            }
        }

        BlockType bt = BT_NONE;
        ColorType ct = CT_NONE;

        BlockType bt2 = BT_NONE;
        ColorType ct2 = CT_NONE;

        //just for performance optimization, since 'GetBlankBlockType' is called too much
        BlockType blank_bt = BT_SQUARE;

        //In one shape, 'open' means two blocks share the wall, so this wall is open to each other;
        //you can imagin that we open a hole in the wall; so we can distinguish one shape from canvas
        //based on 'open' infos(You can imagine that if you pour a dyestuff into a certain cell, then other
        //cells of the shape to which this cell belongs will also be dyed because of these open holes.)
        bool left_open = false;
        bool right_open = false;
        bool up_open = false;
        bool down_open = false;
        bool has_painted_cell_lines = false;
        string cell_lines[3];
    };

    struct OneShot {
        int model_idx = 0;
        int shape_idx = 0;
        int r = 0;
        int c = 0;
    };
    using Snapshot = list<OneShot>;

    static string GetDebugString(const Snapshot& ss, bool sort = true) {
        ostringstream oss;
        if (sort) {
            map<int, const OneShot*> sorted_ss;
            for (auto& shot : ss) sorted_ss[shot.model_idx] = &shot;
            for (auto& x : sorted_ss) {
                oss << "{model_idx:" << x.first << ","
                    << "shape_idx:" << x.second->shape_idx << ","
                    << "r:" << x.second->r << ","
                    << "c:" << x.second->c << "}";
            }
        } else {
            for (auto& shot : ss) {
                oss << "{model_idx:" << shot.model_idx << ","
                    << "shape_idx:" << shot.shape_idx << ","
                    << "r:" << shot.r << ","
                    << "c:" << shot.c << "}";
            }
        }
        return oss.str();
    }

    Canvas(int rows, int cols): canvas_rows(rows), canvas_cols(cols) {
        cells.resize(rows, vector<Cell>(cols));
    }

    inline Cell& GetCell(int r, int c) {
        assert(r >= 0 && r < canvas_rows && "row index is out of range");
        assert(c >= 0 && c < canvas_cols && "column index is out of range");
        return cells[r][c];
    }

    inline const Cell& GetCell(int r, int c) const {
        assert(r >= 0 && r < canvas_rows && "row index is out of range");
        assert(c >= 0 && c < canvas_cols && "column index is out of range");
        return cells[r][c];
    }

    const vector<Cell>& GetRow(int i) const {
        assert(i >= 0 && i < canvas_rows);
        return cells[i];
    }

    int GetCanvasWidth() const {
        return canvas_cols;
    }

    int GetCanvasHeight() const {
        return canvas_rows;
    }

    int GetCanvasAvailableAreaSize() const {
        int res = 0;
        for (auto& r : cells) {
            for (auto& c : r) {
                auto blank_bt = c.GetBlankBlockType();
                if (BT_SQUARE == blank_bt) res += SQUARE_SIZE;
                else if (BT_NONE != blank_bt) res += TRIANGLE_SIZE;
            }
        }
        return res;
    }

    void PaintAllCellLines() {
        for (auto& r : cells) {
            for (auto& c : r) {
                c.PaintCellLines();
            }
        }
    }

    bool HasCellUnpainted() const {
        for (auto& r : cells) {
            for (auto& c : r) {
                if (!c.HasPaintedCellLines() && BT_SQUARE != c.GetBlankBlockType()) {
                    return true;
                }
            }
        }
        return false;
    }

    void Print(bool allow_unpainted_cells = false) const {
        auto print_fn = [&](const Canvas& canvas) {
            for (auto& r : canvas.cells) {
                for (auto& c : r) { printf("%s", c.cell_lines[0].data()); } printf("\n");
                for (auto& c : r) { printf("%s", c.cell_lines[1].data()); } printf("\n");
                for (auto& c : r) { printf("%s", c.cell_lines[2].data()); } printf("\n");
            }
            printf("\n");
        };

        if (!allow_unpainted_cells && HasCellUnpainted()) {
            Canvas c2 = *this;
            c2.PaintAllCellLines();
            print_fn(c2);
        } else {
            print_fn(*this);
        }
    }

    bool Repaint(const vector<Model>& models, const Snapshot& snapshot, bool just_mark = false) {
        canvas_stale = true;
        for (auto& shot : snapshot) {
            auto& m = models[shot.model_idx];
            if (!AddShape(m.GetShapes()[shot.shape_idx], shot.r, shot.c, m.GetModelColor(), just_mark)) return false;
        }
        return true;
    }

    void DedupSnapshots(const vector<Model>& models, vector<Snapshot>& snapshots) const;

    // get the snapshots with minimium dup factor
    static vector<Snapshot> AnalyseDifficultSnapshots(const vector<Snapshot>& snapshots) {
        union ShotKey {
            uint64_t key;
            struct {
                uint16_t model_idx;
                uint16_t shape_idx;
                uint16_t r;
                uint16_t c;
            };
            ShotKey(int model_idx, int shape_idx, int r, int c)
                : model_idx(model_idx), shape_idx(shape_idx), r(r), c(c) {}
            ShotKey(uint64_t key): key(key) {}
        };

        map<uint64_t, int> stats;
        for (auto& ss : snapshots) {
            for (auto& shot : ss) {
                ShotKey sk = {shot.model_idx, shot.shape_idx, shot.r, shot.c};
                stats[sk.key]++;
            }
        }

        int min_score = numeric_limits<int>::max();
        vector<Snapshot> res;
        for (auto& ss : snapshots) {
            int score = 0;
            for (auto& shot : ss) {
                ShotKey sk = {shot.model_idx, shot.shape_idx, shot.r, shot.c};
                score += stats[sk.key];
            }
            if (score < min_score) {
                min_score = score;
                res.clear();
                res.push_back(ss);
            } else if (score == min_score) {
                res.push_back(ss);
            }
        }

        return res;
    }

    bool FillCanvas(const vector<Model>& models) {
        vector<Snapshot> snapshots;
        if (!GetFillCanvasSnapshots(models, snapshots)) return false;
        return Repaint(models, snapshots[0]);
    }

    bool GetFillCanvasSnapshots_Opt(const vector<Model>& models, vector<Snapshot>& snapshots, int max_solutions_num = 1, bool dedup = true) const {
        auto model_less = [&](int a, int b) -> bool {
            auto a_sz = models[a].GetModelAreaSize();
            auto b_sz = models[b].GetModelAreaSize();
            if (a_sz != b_sz) return a_sz < b_sz;
            return a < b;
        };
        //sorted by model area sizes in the ascending order
        set<int, decltype(model_less)> left_models(model_less);

        int canvas_area_size = GetCanvasAvailableAreaSize();
        int all_shapes_area_size = 0;
        for (int i = 0; i <(int)models.size(); ++i) {
            auto sz = models[i].GetModelAreaSize();
            all_shapes_area_size += sz;
            left_models.insert(i);
        }

        bool can_left_blank_block = canvas_area_size > all_shapes_area_size;
        bool can_left_shape = canvas_area_size < all_shapes_area_size;

        Snapshot snapshot;
        Canvas canvas = *this;

        int fn_calls_cnt = 0;
        int solutions_idx = 0;

        vector<vector<WallsInfo>> canvas_walls(canvas_rows, vector<WallsInfo>(canvas_cols));

        int min_shape_size = numeric_limits<int>::max();
        auto analyse_cell_required_type_fn = [&](int r, int c) -> BlockProvidedType {
            auto blank_bt = canvas.cells[r][c].GetBlankBlockType();
            if (BT_NONE == blank_bt) return BPT_NONE;
            const auto& walls = canvas.AnalyseWallsAroundCell(r, c);
            canvas_walls[r][c] = walls;
            int walls_cnt = walls[DT_RIGHT] + walls[DT_LEFT] + walls[DT_UP] + walls[DT_DOWN];
            if (4 == walls_cnt && min_shape_size > SQUARE_SIZE) return BPT_BAD;
            if (BT_SQUARE == blank_bt) {
                if (3 == walls_cnt) return BPT_S_EMBEDED;
                else if (2 == walls_cnt) return BPT_S2_EMBEDED;
            } else {
                switch (blank_bt) {
                case BT_RIGHT_UP:
                    if (walls[DT_RIGHT] && walls[DT_UP]) return BPT_BAD;
                    else if (walls[DT_RIGHT] || walls[DT_UP]) return BPT_TRI_EMBDED;
                    break;
                case BT_RIGHT_DOWN:
                    if (walls[DT_RIGHT] && walls[DT_DOWN]) return BPT_BAD;
                    if (walls[DT_RIGHT] || walls[DT_DOWN]) return BPT_TRI_EMBDED;
                    break;
                case BT_LEFT_UP:
                    if (walls[DT_LEFT] && walls[DT_UP]) return BPT_BAD;
                    if (walls[DT_LEFT] || walls[DT_UP]) return BPT_TRI_EMBDED;
                    break;
                case BT_LEFT_DOWN:
                    if (walls[DT_LEFT] && walls[DT_DOWN]) return BPT_BAD;
                    if (walls[DT_LEFT] || walls[DT_DOWN]) return BPT_TRI_EMBDED;
                    break;
                default: break;
                }
            }
            return BT_SQUARE == blank_bt ? BPT_S_NORMAL : BPT_TRI_NORMAL;
        };

        vector<vector<BlockProvidedType>> canvas_bpts(canvas_rows, vector<BlockProvidedType>(canvas_cols));
        union CellPos {
            int64_t pos;
            struct {
                int r;
                int c;
            };
            CellPos(int r, int c): r(r), c(c){}
            CellPos(int64_t pos): pos(pos) {}
        };
        //it could be merged into canvas_bpts, but reserve it for performance optimization
        unordered_set<int64_t> left_cells;

        auto cell_less = [&](const pair<int, int>& a, const pair<int, int>& b) -> bool {
            auto bpt_a = canvas_bpts[a.first][a.second];
            auto bpt_b = canvas_bpts[b.first][b.second];
            bool special_case = (12 == bpt_a * bpt_b);
            if (!special_case && bpt_a != bpt_b) return bpt_a < bpt_b;

            auto get_surrounding_space_size_fn = [&](int r, int c) -> int {
                auto get_cell_space_size_fn = [&](int r, int c, DirectionType from_dt) -> int {
                    auto blank_bt = canvas.cells[r][c].GetBlankBlockType();
                    switch (blank_bt) {
                    case BT_SQUARE: return SQUARE_SIZE;
                    case BT_RIGHT_DOWN: return (DT_RIGHT == from_dt || DT_DOWN == from_dt) ? TRIANGLE_SIZE : 0;
                    case BT_RIGHT_UP: return (DT_RIGHT == from_dt || DT_UP == from_dt) ? TRIANGLE_SIZE : 0;
                    case BT_LEFT_DOWN: return (DT_LEFT == from_dt || DT_DOWN == from_dt) ? TRIANGLE_SIZE : 0;
                    case BT_LEFT_UP: return (DT_LEFT == from_dt || DT_UP == from_dt) ? TRIANGLE_SIZE : 0;
                    default: break;
                    }
                    return 0;
                };
                int res = 0;
                auto blank_bt = canvas.cells[r][c].GetBlankBlockType();
                switch (blank_bt) {
                case BT_SQUARE:
                    res += SQUARE_SIZE;
                    if (r > 0) res += get_cell_space_size_fn(r - 1, c, DT_DOWN);
                    if (c > 0) res += get_cell_space_size_fn(r, c - 1, DT_RIGHT);
                    if (r < canvas_rows - 1) res += get_cell_space_size_fn(r + 1, c, DT_UP);
                    if (c < canvas_cols - 1) res += get_cell_space_size_fn(r, c + 1, DT_LEFT);
                    break;
                case BT_RIGHT_DOWN:
                    res += TRIANGLE_SIZE;
                    if (c < canvas_cols - 1) res += get_cell_space_size_fn(r, c + 1, DT_LEFT);
                    if (r < canvas_rows - 1) res += get_cell_space_size_fn(r + 1, c, DT_UP);
                    break;
                case BT_RIGHT_UP:
                    res += TRIANGLE_SIZE;
                    if (c < canvas_cols - 1) res += get_cell_space_size_fn(r, c + 1, DT_LEFT);
                    if (r > 0) res += get_cell_space_size_fn(r - 1, c, DT_DOWN);
                    break;
                case BT_LEFT_DOWN:
                    res += TRIANGLE_SIZE;
                    if (c > 0) res += get_cell_space_size_fn(r, c - 1, DT_RIGHT);
                    if (r < canvas_rows - 1) res += get_cell_space_size_fn(r + 1, c, DT_UP);
                    break;
                case BT_LEFT_UP:
                    res += TRIANGLE_SIZE;
                    if (c > 0) res += get_cell_space_size_fn(r, c - 1, DT_RIGHT);
                    if (r > 0) res += get_cell_space_size_fn(r - 1, c, DT_DOWN);
                    break;
                default: break;
                }
                return res;
            };

            auto check_corner_fn = [&](int r, int c) {
                return (0 == r && 0 == c) || (0 == r && canvas_cols - 1 == c)
                    || (canvas_rows - 1 == r && 0 == c) || (canvas_rows - 1 == r && canvas_cols - 1 == c);
            };

            auto sur_area_sz_a = get_surrounding_space_size_fn(a.first, a.second);
            auto sur_area_sz_b = get_surrounding_space_size_fn(b.first, b.second);
            if (special_case) {
                if (BPT_TRI_NORMAL == bpt_a) {
                    if (SQUARE_SIZE * 3 == sur_area_sz_b) {
                        if (sur_area_sz_a < SQUARE_SIZE * 2) return true;
                        else if (sur_area_sz_a <= SQUARE_SIZE * 2 && check_corner_fn(b.first, b.second)) return true;
                    }
                } else { //BPT_TRI_NORMAL == bpt_b
                    if (SQUARE_SIZE * 3 == sur_area_sz_a) {
                        if (sur_area_sz_b < SQUARE_SIZE * 2) return false;
                        else if (sur_area_sz_b <= SQUARE_SIZE * 2 && check_corner_fn(a.first, a.second)) return false;
                    }
                }
                return bpt_a < bpt_b;
            }
            if (sur_area_sz_a != sur_area_sz_b) return sur_area_sz_a < sur_area_sz_b;

            auto get_surrounding_bpt_sum_fn = [&](int r, int c) {
                auto get_bpt_fn = [&](int r, int c) -> int {
                    auto bpt = canvas_bpts[r][c];
                    return BPT_NONE == bpt ? 0 : bpt;
                };
                int res = 0;
                if (r > 0) res += get_bpt_fn(r - 1, c);
                if (c > 0) res += get_bpt_fn(r, c - 1);
                if (r < canvas_rows - 1) res += get_bpt_fn(r + 1, c);
                if (c < canvas_cols - 1) res += get_bpt_fn(r, c + 1);
                return res;
            };
            auto sur_bpt_a = get_surrounding_bpt_sum_fn(a.first, a.second);
            auto sur_bpt_b = get_surrounding_bpt_sum_fn(b.first, b.second);
            if (sur_bpt_a != sur_bpt_b) return sur_bpt_a < sur_bpt_b;

            //prefer non-corner cells
            if (check_corner_fn(a.first, a.second)) {
                if (!check_corner_fn(b.first, b.second)) return false;
            } else if (check_corner_fn(b.first, b.second)) return true;

            return a < b;
        };

        for (int i = 0; i < canvas_rows; ++i) {
            for (int j = 0; j < canvas_cols; ++j) {
                auto bpt = analyse_cell_required_type_fn(i, j);
                canvas_bpts[i][j] = bpt;
                if (BPT_NONE != bpt) left_cells.insert(CellPos(i, j).pos);
            }
        }

        int tough_r = -1, tough_c = -1;
        auto update_tough_cell_fn = [&]() {
            tough_r = -1; tough_c = -1;
            for (auto x : left_cells) {
                CellPos pos(x);
                if ((-1 == tough_r && -1 == tough_c)) {
                    tough_r = pos.r; tough_c = pos.c;
                } else if (0 == canvas_walls[pos.r][pos.c].GetWallsCount()) {
                    continue;
                } else if (cell_less({pos.r, pos.c}, {tough_r, tough_c})) {
                    tough_r = pos.r; tough_c = pos.c;
                }
            }
        };
        update_tough_cell_fn();

        //it can be applied for adding/removeing a shape
        auto adjust_cell_requires_for_shape_fn = [&](const Shape& s, int sr, int sc) -> bool {
            //the process must be split into two steps since 'cell_less' will use the info of adjacent cells:
            //  1. update canvas_bpts;
            //  2. find tough cell;
            for (auto& x : s.GetBlocksWithOutline()) {
                int r = x.first + sr;
                int c = x.second + sc;
                if (r < 0 || c < 0 || r > canvas_rows - 1 || c > canvas_cols - 1) continue;
                const auto new_bpt = analyse_cell_required_type_fn(r, c);
                if (BPT_BAD == new_bpt) return false;
                canvas_bpts[r][c] = new_bpt;
                if (BPT_NONE == new_bpt) left_cells.erase(CellPos(r, c).pos);
                else left_cells.insert(CellPos(r, c).pos);
            }

            update_tough_cell_fn();
            return true;
        };

        using Solution = bitset<20>;
        assert(models.size() < Solution().size());
        map<int, vector<Solution>> lz_solutions;
        for (int idx : left_models) {
            auto sz = models[idx].GetModelAreaSize();
            for (auto iter = lz_solutions.begin(); lz_solutions.end() != iter; ++iter) {
                int lz = iter->first;
                for (auto slt : iter->second) { //slt must be a copy obj, can't use 'auto&' here
                    if (slt[idx]) break;
                    lz_solutions[lz + sz].push_back(move(slt.set(idx)));
                }
            }
            lz_solutions[sz].push_back(move(Solution().set(idx)));
        }

        auto get_lake_solutions_fn = [&](int sz, const Solution& ignored) {
            vector<Solution> res;
            for (auto& slt : lz_solutions[sz]) {
                if ((slt & ignored).any()) continue;
                res.push_back(slt);
            }
            return res;
        };

        auto get_all_lake_sizes_fn = [&]() -> vector<int> {
            auto left = left_cells;
            std::function<int(int, int)> get_lake_area_size_fn;
            get_lake_area_size_fn = [&](int r, int c) -> int {
                if (r < 0 || c < 0 || r > canvas_rows - 1 || c > canvas_cols - 1) return 0;

                auto iter = left.find(CellPos(r, c).pos);
                if (left.end() == iter) return 0;
                left.erase(iter);
                auto blank_bt = canvas.cells[r][c].GetBlankBlockType();
                int res = 0;

                const auto& walls = canvas_walls[r][c];
                if (BT_SQUARE == blank_bt) {
                    res = SQUARE_SIZE;
                    if (!walls[DT_UP]) res += get_lake_area_size_fn(r - 1, c);
                    if (!walls[DT_DOWN]) res += get_lake_area_size_fn(r + 1, c);
                    if (!walls[DT_LEFT]) res += get_lake_area_size_fn(r, c - 1);
                    if (!walls[DT_RIGHT]) res += get_lake_area_size_fn(r, c + 1);
                } else {
                    res = TRIANGLE_SIZE;
                    switch (blank_bt) {
                    case BT_RIGHT_UP:
                        if (!walls[DT_RIGHT]) res += get_lake_area_size_fn(r, c + 1);
                        if (!walls[DT_UP]) res += get_lake_area_size_fn(r - 1, c);
                        break;
                    case BT_RIGHT_DOWN:
                        if (!walls[DT_RIGHT]) res += get_lake_area_size_fn(r, c + 1);
                        if (!walls[DT_DOWN]) res += get_lake_area_size_fn(r + 1, c);
                        break;
                    case BT_LEFT_UP:
                        if (!walls[DT_LEFT]) res += get_lake_area_size_fn(r, c - 1);
                        if (!walls[DT_UP]) res += get_lake_area_size_fn(r - 1, c);
                        break;
                    case BT_LEFT_DOWN:
                        if (!walls[DT_LEFT]) res += get_lake_area_size_fn(r, c - 1);
                        if (!walls[DT_DOWN]) res += get_lake_area_size_fn(r + 1, c);
                        break;
                    default: break;
                    }
                }

                return res;
            };

            Solution ignored;
            for (int i = 0; i < (int)models.size(); ++i) {
                if (left_models.end() == left_models.find(i)) ignored[i] = true;
            }

            vector<int> res;
            while (!left.empty()) {
                CellPos pos(*left.begin());
                auto lz = get_lake_area_size_fn(pos.r, pos.c);
                const auto& slts = get_lake_solutions_fn(lz, ignored);
                if (1 == slts.size() && 1 == slts[0].count()) {
                    tough_r = pos.r;
                    tough_c = pos.c;
                }
                res.push_back(lz);
            }
            return res;
        };

        auto try_fulfill_all_lake_sizes_fn = [&](const vector<int>& lake_sizes) {
            function<bool(int, Solution&)> helper_fn;
            helper_fn = [&](int start_lake_idx, Solution& ignored) {
                if (start_lake_idx + 1 == (int)lake_sizes.size() || ignored.size() == left_models.size()) return true;
                auto lz = lake_sizes[start_lake_idx];
                const auto& res = get_lake_solutions_fn(lz, ignored);
                for (auto& slt : res) {
                    ignored |= slt;
                    if (helper_fn(start_lake_idx + 1, ignored)) return true;
                    ignored &= ~slt;
                }
                return false;
            };

            Solution ignored;
            for (int i = 0; i < (int)models.size(); ++i) {
                if (left_models.end() == left_models.find(i)) ignored[i] = true;
            }
            return helper_fn(0, ignored);
        };

        function<bool()> fill_left_canvas_fn;
        fill_left_canvas_fn = [&]() -> bool {
            if (left_cells.empty()) {
                if (!can_left_shape && !left_models.empty()) return false;
                return true;
            }
            if (can_left_blank_block && left_models.empty()) return true;

            int i = tough_r;
            int j = tough_c;

            auto blank_bt = canvas.cells[i][j].GetBlankBlockType();
            const auto& walls = canvas_walls[i][j];
            int walls_cnt = walls[DT_RIGHT] + walls[DT_LEFT] + walls[DT_UP] + walls[DT_DOWN];

            auto fill_current_block_fn = [&](BlockType bt, bool refill_current_block = true) -> bool {
                ++fn_calls_cnt;
                for (auto iter = left_models.begin(); iter != left_models.end(); ++iter) {
                    int idx = *iter;
                    auto& m = models[idx];
                    const auto& shapes = m.GetShapes();

                    const set<pair<int, int>>* offers = &m.offers[bt];
                    if (BT_SQUARE == bt) {
                        if (3 == walls_cnt) {
                            if (!walls[DT_DOWN]) offers = &m.s_d_embeded_offers;
                            else if (!walls[DT_UP]) offers = &m.s_u_embeded_offers;
                            else if (!walls[DT_RIGHT]) offers = &m.s_r_embeded_offers;
                            else if (!walls[DT_LEFT]) offers = &m.s_l_embeded_offers;
                        } else if (2 == walls_cnt) {
                            if (!walls[DT_RIGHT] && !walls[DT_LEFT]) offers = &m.s_lr_embeded_offers;
                            else if (!walls[DT_UP] && !walls[DT_DOWN]) offers = &m.s_ud_embeded_offers;
                            else if (!walls[DT_RIGHT] && !walls[DT_UP]) offers = &m.s_ru_offers;
                            else if (!walls[DT_RIGHT] && !walls[DT_DOWN]) offers = &m.s_rd_offers;
                            else if (!walls[DT_LEFT] && !walls[DT_UP]) offers = &m.s_lu_offers;
                            else if (!walls[DT_LEFT] && !walls[DT_DOWN]) offers = &m.s_ld_offers;
                        }
                    } else {
                        if (walls_cnt >= 2) {
                            switch (bt) {
                            case BT_RIGHT_UP:
                                if (walls[DT_RIGHT]) offers = &m.ru_u_embeded_offers;
                                else if (walls[DT_UP]) offers = &m.ru_r_embeded_offers;
                                break;
                            case BT_RIGHT_DOWN:
                                if (walls[DT_RIGHT]) offers = &m.rd_d_embeded_offers;
                                else if (walls[DT_DOWN]) offers = &m.rd_r_embeded_offers;
                                break;
                            case BT_LEFT_UP:
                                if (walls[DT_LEFT]) offers = &m.lu_u_embeded_offers;
                                else if (walls[DT_UP]) offers = &m.lu_l_embeded_offers;
                                break;
                            case BT_LEFT_DOWN:
                                if (walls[DT_LEFT]) offers = &m.ld_d_embeded_offers;
                                else if (walls[DT_DOWN]) offers = &m.ld_l_embeded_offers;
                                break;
                            default: break;
                            }
                        }
                    }

                    left_models.erase(idx);
                    for (auto& x : *offers) {
                        const auto& s = shapes[x.first];
                        int r = i - s[x.second].rel_r;
                        int c = j - s[x.second].rel_c;
                        if (r < 0 || c < 0 || r >= canvas_rows || c >= canvas_cols) continue;
                        if (canvas.AddShape(s, r, c, m.GetModelColor(), true)) {
                            canvas_area_size -= s.GetShapeAreaSize();
                            snapshot.push_back({idx, x.first, r, c});

                            bool fill_left_ok = false;
                            min_shape_size = left_models.empty() ? 0 : models[*left_models.begin()].GetModelAreaSize();
                            int min_2nd_shape_size = left_models.size() > 1 ? models[*(++left_models.begin())].GetModelAreaSize() : 0;
                            set<int> left_shape_sizes;
                            for (auto x : left_models) left_shape_sizes.insert(models[x].GetModelAreaSize());
                            if (adjust_cell_requires_for_shape_fn(s, r, c)) {
                                bool can_stop_detect_further = false;

                                if (!can_left_blank_block && canvas_area_size > 0) {
                                    //maybe update tough position within get_all_lake_sizes_fn
                                    const auto& lake_sizes = get_all_lake_sizes_fn();
                                    for (auto lake_sz : lake_sizes) {
                                        if (lake_sz < min_shape_size) {
                                            can_stop_detect_further = true;
                                            break;
                                        } else if (lake_sz > min_shape_size && lake_sz < min_shape_size + min_2nd_shape_size &&
                                                left_shape_sizes.end() == left_shape_sizes.find(lake_sz)) {
                                            can_stop_detect_further = true;
                                            break;
                                        }
                                    }
                                    if (!can_stop_detect_further && !try_fulfill_all_lake_sizes_fn(lake_sizes)) {
                                        can_stop_detect_further = true;
                                    }

                                    if (!can_stop_detect_further && !can_left_shape) {
                                        int needed_s_embeded_cnt = 0;
                                        int needed_tri_embeded_cnt = 0;
                                        int needed_s2_embeded_cnt = 0;
                                        for (auto x : left_cells) {
                                            CellPos pos(x);
                                            auto bpt = canvas_bpts[pos.r][pos.c];
                                            if (BPT_S_EMBEDED == bpt) ++needed_s_embeded_cnt;
                                            else if (BPT_TRI_EMBDED == bpt) ++needed_tri_embeded_cnt;
                                            else if (BPT_S2_EMBEDED == bpt) ++needed_s2_embeded_cnt;
                                        }
                                        int provided_s_embeded_cnt = 0;
                                        int provided_tri_embeded_cnt = 0;
                                        int provided_s2_embeded_cnt = 0;
                                        for (auto x : left_models) {
                                            provided_s_embeded_cnt += models[x].GetShapes()[0].s_embeded_blk_cnt;
                                            provided_tri_embeded_cnt += models[x].GetShapes()[0].tri_embeded_blk_cnt;
                                            provided_s2_embeded_cnt += models[x].GetShapes()[0].s2_embeded_blk_cnt;
                                        }
                                        if (needed_s_embeded_cnt > provided_s_embeded_cnt || needed_tri_embeded_cnt > provided_tri_embeded_cnt) {
                                            can_stop_detect_further = true;
                                        }
                                        if (needed_s_embeded_cnt + needed_s2_embeded_cnt >
                                                provided_s_embeded_cnt + provided_s2_embeded_cnt + (provided_tri_embeded_cnt - needed_tri_embeded_cnt) / 2) {
                                            can_stop_detect_further = true;
                                        }
                                    }
                                }

                                if (!can_stop_detect_further) {
#ifdef DEBUG
                                    static int s_counter = 0;
                                    if (s_counter > 0 && s_counter-- > 0) { //debug the detecting process
                                        printf("======= add one shape at (%d, %d) %s ======\n", i, j, GetDebugString(snapshot, false).data());
                                        vector<Canvas> cs;
                                        Canvas c2 = *this; c2.Repaint(models, snapshot); cs.push_back(move(c2));
                                        for (auto idx : left_models) {
                                            auto& s = models[idx].GetShapes()[0];
                                            Canvas c(canvas_rows, s.GetCircumRectangleSize().second);
                                            c.AddShape(s, -1, -1, models[idx].GetModelColor());
                                            cs.push_back(move(c));
                                        }
                                        PrintCanvases(cs);
                                    }
#endif

                                    bool need_recover = false;
                                    auto iter = left_cells.find(CellPos(i, j).pos);
                                    if (!refill_current_block && left_cells.end() != iter) {
                                        need_recover = true;
                                        left_cells.erase(iter);
                                    }
                                    fill_left_ok = fill_left_canvas_fn();
                                    if (need_recover) left_cells.insert(CellPos(i, j).pos);
                                }
                            }

                            assert(canvas.EraseShapeMark(s, r, c));
                            canvas_area_size += s.GetShapeAreaSize();
                            adjust_cell_requires_for_shape_fn(s, r, c);

                            if (fill_left_ok) {
                                if (solutions_idx++ < max_solutions_num || max_solutions_num < 0) {
                                    snapshots.push_back(snapshot);
#ifdef DEBUG
                                    Canvas c2 = *this;
                                    c2.Repaint(models, snapshot);
                                    c2.Print();
                                    static int last_fn_calls_cnt = 0;
                                    printf("fn_calls_cnt:%d, diff:%d, solution[%d]: %s\n", fn_calls_cnt, fn_calls_cnt - last_fn_calls_cnt,
                                            solutions_idx - 1, GetDebugString(snapshot).data());
                                    last_fn_calls_cnt = fn_calls_cnt;
#endif
                                    if (solutions_idx < max_solutions_num || max_solutions_num < 0) fill_left_ok = false;
                                }
                            }

                            if (!fill_left_ok) {
                                snapshot.pop_back();
                                continue;
                            }
                            return true;
                        }
                    }
                    iter = left_models.insert(idx).first;
                }
                return false;
            };

            if (fill_current_block_fn(blank_bt)) return true;
            if (BT_SQUARE == blank_bt) {
                if (fill_current_block_fn(BT_RIGHT_UP) || fill_current_block_fn(BT_RIGHT_DOWN)) return true;
            }

            if (can_left_blank_block) {
                if (BT_SQUARE == blank_bt) {
                    if (fill_current_block_fn(BT_RIGHT_UP, false) || fill_current_block_fn(BT_RIGHT_DOWN, false) ||
                        fill_current_block_fn(BT_LEFT_UP, false) || fill_current_block_fn(BT_LEFT_DOWN, false)) {
                        return true;
                    }
                }
                left_cells.erase(CellPos(i, j).pos);
                return fill_left_canvas_fn();
            }

            return false;
        };

        fill_left_canvas_fn();
#ifdef DEBUG
        printf("fn_calls_cnt:%d\n", fn_calls_cnt);
#endif

        if (dedup) DedupSnapshots(models, snapshots);
        return !snapshots.empty();
    }

    bool GetFillCanvasSnapshots(const vector<Model>& models, vector<Snapshot>& snapshots, int max_solutions_num = 1, bool dedup = true) const {
        int canvas_area_size = GetCanvasAvailableAreaSize();
        int all_shapes_area_size = 0;
        for (auto& m : models) all_shapes_area_size += m.GetModelAreaSize();
        bool can_left_blank_block = canvas_area_size > all_shapes_area_size;
        bool can_left_shape = canvas_area_size < all_shapes_area_size;

        Canvas canvas = *this;

        Snapshot snapshot;
        set<int> left_model_idxs;
        for (int i = 0; i <(int)models.size(); ++i) left_model_idxs.insert(i);

        int fn_calls_cnt = 0;
        int solutions_idx = 0;

        function<bool(int, int)> fill_left_canvas_fn;
        fill_left_canvas_fn = [&](int i, int j) -> bool {
            if (j >= canvas_cols) j = 0, ++i;
            if (i >= canvas_rows) {
                if (!can_left_shape && !left_model_idxs.empty()) return false;
                return true;
            }
            if (can_left_blank_block && left_model_idxs.empty()) return true;

            auto blank_bt = canvas.cells[i][j].GetBlankBlockType();
            if (BT_NONE == blank_bt) return fill_left_canvas_fn(i, j + 1);
            const auto& walls = canvas.AnalyseWallsAroundCell(i, j);
            int walls_cnt = walls[DT_RIGHT] + walls[DT_LEFT] + walls[DT_UP] + walls[DT_DOWN];

            auto fill_current_block_fn = [&](BlockType bt, bool refill_current_block = false) -> bool {
                ++fn_calls_cnt;
                for (auto iter = left_model_idxs.begin(); iter != left_model_idxs.end(); ++iter) {
                    int idx = *iter;
                    auto& m = models[idx];
                    const auto& shapes = m.GetShapes();

                    const set<pair<int, int>>* offers = &m.offers[bt];
                    if (BT_SQUARE == bt) {
                        if (3 == walls_cnt) {
                            if (!walls[DT_DOWN]) offers = &m.s_d_embeded_offers;
                            else if (!walls[DT_UP]) offers = &m.s_u_embeded_offers;
                            else if (!walls[DT_RIGHT]) offers = &m.s_r_embeded_offers;
                            else if (!walls[DT_LEFT]) offers = &m.s_l_embeded_offers;
                        } else if (2 == walls_cnt) {
                            if (!walls[DT_RIGHT] && !walls[DT_LEFT]) offers = &m.s_lr_embeded_offers;
                            else if (!walls[DT_UP] && !walls[DT_DOWN]) offers = &m.s_ud_embeded_offers;
                            else if (!walls[DT_RIGHT] && !walls[DT_UP]) offers = &m.s_ru_offers;
                            else if (!walls[DT_RIGHT] && !walls[DT_DOWN]) offers = &m.s_rd_offers;
                            else if (!walls[DT_LEFT] && !walls[DT_UP]) offers = &m.s_lu_offers;
                            else if (!walls[DT_LEFT] && !walls[DT_DOWN]) offers = &m.s_ld_offers;
                        }
                    } else {
                        if (walls_cnt >= 2) {
                            switch (bt) {
                            case BT_RIGHT_UP:
                                if (walls[DT_RIGHT]) offers = &m.ru_u_embeded_offers;
                                else if (walls[DT_UP]) offers = &m.ru_r_embeded_offers;
                                break;
                            case BT_RIGHT_DOWN:
                                if (walls[DT_RIGHT]) offers = &m.rd_d_embeded_offers;
                                else if (walls[DT_DOWN]) offers = &m.rd_r_embeded_offers;
                                break;
                            case BT_LEFT_UP:
                                if (walls[DT_LEFT]) offers = &m.lu_u_embeded_offers;
                                else if (walls[DT_UP]) offers = &m.lu_l_embeded_offers;
                                break;
                            case BT_LEFT_DOWN:
                                if (walls[DT_LEFT]) offers = &m.ld_d_embeded_offers;
                                else if (walls[DT_DOWN]) offers = &m.ld_l_embeded_offers;
                                break;
                            default: break;
                            }
                        }
                    }

                    left_model_idxs.erase(idx);
                    for (auto& x : *offers) {
                        const auto& s = shapes[x.first];
                        int r = i - s[x.second].rel_r;
                        int c = j - s[x.second].rel_c;
                        if (r < 0 || c < 0 || r >= canvas_rows || c >= canvas_cols) continue;
                        if (canvas.AddShape(s, r, c, m.GetModelColor(), true)) {
                            snapshot.push_back({idx, x.first, r, c});

                            bool ret = fill_left_canvas_fn(i, j + !refill_current_block);
                            assert(canvas.EraseShapeMark(s, r, c));

                            if (ret) {
                                if (solutions_idx++ < max_solutions_num || max_solutions_num < 0) {
                                    snapshots.push_back(snapshot);
#ifdef DEBUG
                                    Canvas c2(canvas_rows, canvas_cols);
                                    for (auto& shot : snapshot) {
                                        assert(c2.AddShape(models[shot.model_idx].GetShapes()[shot.shape_idx], shot.r, shot.c, models[shot.model_idx].GetModelColor(), false));
                                    }
                                    c2.Print();
                                    printf("fn_calls_cnt:%d, solution[%d]: %s\n", fn_calls_cnt, solutions_idx - 1, GetDebugString(snapshot).data());
#endif

                                    if (solutions_idx < max_solutions_num || max_solutions_num < 0) ret = false;
                                }
                            }

                            if (!ret) {
                                snapshot.pop_back();
                                continue;
                            }
                            return true;
                        }
                    }
                    iter = left_model_idxs.insert(idx).first;
                }
                return false;
            };

            if (fill_current_block_fn(blank_bt)) return true;
            if (BT_SQUARE == blank_bt) {
                if (fill_current_block_fn(BT_RIGHT_UP, true) || fill_current_block_fn(BT_RIGHT_DOWN, true)) return true;
            }

            if (can_left_blank_block) {
                if (BT_SQUARE == blank_bt) {
                    if (fill_current_block_fn(BT_RIGHT_UP) || fill_current_block_fn(BT_RIGHT_DOWN) ||
                        fill_current_block_fn(BT_LEFT_UP) || fill_current_block_fn(BT_LEFT_DOWN)) {
                        return true;
                    }
                }
                return fill_left_canvas_fn(i, j + 1);
            }

            return false;
        };

        fill_left_canvas_fn(0, 0);
#ifdef DEBUG
        printf("fn_calls_cnt:%d\n", fn_calls_cnt);
#endif

        if (dedup) DedupSnapshots(models, snapshots);
        return !snapshots.empty();
    }

    // the first brute-force version, very slow
    bool AddModels(const vector<Model>& models) {
        bool dedup = false;
        int max_solutions_num = 1;
        vector<Snapshot> all_snapshots;

        std::mutex mtx;
        function<int(Canvas&, int, int, Snapshot&)> helper_fn;
        helper_fn = [&](Canvas& canvas, int model_idx, int fixed_shape_idx, Snapshot& snapshot) -> int {
            if (max_solutions_num <= 0) return -1;
            if (model_idx >= (int)models.size()) return 1;
            const auto& m = models[model_idx];
            const auto& shapes = m.GetShapes();
            for (int shape_idx = 0; shape_idx < (int)shapes.size(); ++shape_idx) {
                if (fixed_shape_idx >= 0 && shape_idx != fixed_shape_idx) continue;
                const auto& s = shapes[shape_idx];
                for (int i = 0; i < canvas_rows; ++i) {
                    for (int j = 0; j < canvas_cols; ++j) {
                        if (!canvas.cells[i][j].CanPaintBlock(s[0].bt)) continue;
                        if (canvas.AddShape(s, i, j, m.GetModelColor(), true)) {
                            snapshot.push_back({model_idx, shape_idx, i, j});
                            int ret = helper_fn(canvas, model_idx + 1, -1, snapshot);
                            assert(canvas.EraseShapeMark(s, i, j));
                            if (1 == ret) {
                                std::lock_guard<std::mutex> guard(mtx);
                                if (max_solutions_num < 0 || max_solutions_num > 0) {
                                    --max_solutions_num;
                                    all_snapshots.push_back(snapshot);
                                    ret = 0;
                                }
                            }
                            if (0 == ret) {
                                snapshot.pop_back();
                                continue;
                            }
                            return ret;
                        }
                    }
                }
            }
            return 0;
        };

        vector<thread> threads;
        int shapes_sz = models[0].GetShapes().size();
        vector<Snapshot> snapshots(shapes_sz);
        vector<Canvas> canvases(shapes_sz, {canvas_rows, canvas_cols});
        for (int i = 0; i < shapes_sz; ++i) {
            canvases[i] = *this;
            threads.emplace_back(helper_fn, ref(canvases[i]), 0, i, ref(snapshots[i]));
        }
        for (auto& t : threads) t.join();

        if (dedup) DedupSnapshots(models, snapshots);

        if (all_snapshots.size() == 1) {
            assert(Repaint(models, all_snapshots[0]));
        }
        return !all_snapshots.empty();
    }

    bool CanPlaceShape(const Shape& s, int r, int c) const {
        vector<bool> visited(s.size());
        std::function<bool(int, int, int)> fn;
        fn = [&](int idx, int r, int c) -> bool {
            visited[idx] = true;
            auto& b = s[idx];
            if (r < 0 || r >= canvas_rows) return false;
            if (c < 0 || c >= canvas_cols) return false;
            if (!cells[r][c].CanPaintBlock(b.bt)) return false;
            if (b.up >= 0 && !visited[b.up]) if (!fn(b.up, r - 1, c)) return false;
            if (b.down >= 0 && !visited[b.down]) if (!fn(b.down, r + 1, c)) return false;
            if (b.left >= 0 && !visited[b.left]) if (!fn(b.left, r, c - 1)) return false;
            if (b.right >= 0 && !visited[b.right]) if (!fn(b.right, r , c + 1)) return false;
            return true;
        };
        return fn(0, r, c);
    }

    bool EraseShapeMark(const Shape& s, int r, int c) {
        canvas_stale = true;
        vector<bool> visited(s.size());
        std::function<bool(int, int, int)> fn;
        fn = [&](int idx, int r, int c) -> bool {
            visited[idx] = true;
            auto& b = s[idx];
            if (!cells[r][c].EraseBlockMark(b.bt)) return false;
            if (b.up >= 0 && !visited[b.up]) if (!fn(b.up, r - 1, c)) return false;
            if (b.down >= 0 && !visited[b.down]) if (!fn(b.down, r + 1, c)) return false;
            if (b.left >= 0 && !visited[b.left]) if (!fn(b.left, r, c - 1)) return false;
            if (b.right >= 0 && !visited[b.right]) if (!fn(b.right, r , c + 1)) return false;
            return true;
        };

        return fn(0, r, c);
    }

    bool AddShape(const Shape& s, int r = -1, int c = -1, ColorType ct = CT_NONE, bool just_mark = false, bool blk0_with_diff_color = false) {
        canvas_stale = true;
        if (CT_NONE == ct) {
            for (int i = CT_NONE + 1, k = rand(); i < CT_MAX; ++i) {
                int j = 1 + (i + k) % (CT_MAX - 1);
                if (used_colors.find(j) == used_colors.end()) {
                    ct = (ColorType)j;
                    break;
                }
            }
            if (CT_NONE == ct) ct = (ColorType)(1 + rand() % (CT_MAX - 1));
        }
        if (!just_mark) used_colors.insert(ct);

        if (r < 0 || c < 0) {
            //seek position that can be used to paint the whole shape
            [&]() {
                for (int i = 0; i < canvas_rows; ++i) {
                    for (int j = 0; j < canvas_cols; ++j) {
                        if (CanPlaceShape(s, i, j)) {
                            r = i;
                            c = j;
                            return;
                        }
                    }
                }
            }();
            if (r < 0 || c < 0) {
                return false;
            }
        } else if (r >= canvas_rows || c >= canvas_cols) {
            return false;
        } else if (!CanPlaceShape(s, r, c)) {
            return false;
        }

        vector<bool> visited(s.size());
        std::function<void(int, int, int)> fn;
        fn = [&](int idx, int r, int c) {
            visited[idx] = true;
            auto& b = s[idx];
            auto& cell = cells[r][c];
            assert(cell.PaintBlock(b.bt, 0 == idx && blk0_with_diff_color ? (ColorType)(CT_MAX == ct + 1 ? 1 : ct + 1) : ct, just_mark));
            if (b.up >= 0) {
                cell.up_open = true;
                cells[r - 1][c].down_open = true;
                if (!visited[b.up]) fn(b.up, r - 1, c);
            }
            if (b.down >= 0) {
                cell.down_open = true;
                cells[r + 1][c].up_open = true;
                if (!visited[b.down]) fn(b.down, r + 1, c);
            }
            if (b.left >= 0) {
                cell.left_open = true;
                cells[r][c - 1].right_open = true;
                if (!visited[b.left]) fn(b.left, r, c - 1);
            }
            if (b.right >= 0) {
                cell.right_open = true;
                cells[r][c + 1].left_open = true;
                if (!visited[b.right]) fn(b.right, r , c + 1);
            }
        };

        fn(0, r, c);
        return true;
    }

    //Fill all cells, then take out the shape area
    void ReverseShape(const Shape& s, int r, int c) {
        canvas_stale = true;
        for (int i = 0; i < canvas_rows; ++i) {
            for (int j = 0; j < canvas_cols; ++j) {
                assert(cells[i][j].PaintBlock(BT_SQUARE, CT_NONE, true));
            }
        }
        assert(EraseShapeMark(s, r, c));
    }

    template <bool JUST_CHECK_OCCUPIED = false>
    struct CanvasHash {
        static size_t Hash(const Canvas& c, size_t* val2 = nullptr) {
            int rows = c.GetCanvasHeight();
            int cols = c.GetCanvasWidth();
            string s(rows * cols * (JUST_CHECK_OCCUPIED ? 1 : 5), 0);
            int idx = 0;
            for (int i = 0; i < rows; ++i) {
                for (int j = 0; j < cols; ++j) {
                    auto& cell = c.cells[i][j];
                    if (BT_NONE == cell.bt) s[idx++] = 0;
                    else if (BT_NONE == cell.bt2) s[idx++] = cell.bt;
                    else s[idx++] = JUST_CHECK_OCCUPIED ? BT_SQUARE : cell.bt * cell.bt2;
                    if (!JUST_CHECK_OCCUPIED) {
                        s[idx++] = cell.left_open;
                        s[idx++] = cell.right_open;
                        s[idx++] = cell.up_open;
                        s[idx++] = cell.down_open;
                    }
                }
            }

            if (val2) *val2 = hash<string>()(s + string_printf("%dx%d", rows, cols));
            return hash<string>()(string_printf("%dx%d", rows, cols) + s);
        }

        static void UpdateCanvas(Canvas& c) {
            if (JUST_CHECK_OCCUPIED) {
                c.hash_val_just_check_occupied = Hash(c, &c.hash_val2_just_check_occupied);
            } else {
                c.hash_val = Hash(c, &c.hash_val2);
            }
            c.canvas_stale = false;
        }

        std::size_t operator()(const Canvas& c) const {
            if (c.canvas_stale) UpdateCanvas(const_cast<Canvas&>(c));
            return JUST_CHECK_OCCUPIED ? c.hash_val_just_check_occupied : c.hash_val;
        }
    };

    template <bool JUST_CHECK_OCCUPIED>
    struct CanvasEqual {
        bool operator()(const Canvas& c1, const Canvas& c2) const {
            if (c1.canvas_rows != c2.canvas_rows || c1.canvas_cols != c2.canvas_cols) return false;
            if (JUST_CHECK_OCCUPIED) {
                if (c1.hash_val_just_check_occupied != c2.hash_val_just_check_occupied ||
                        c1.hash_val2_just_check_occupied != c2.hash_val2_just_check_occupied) return false;
            } else {
                if (c1.hash_val != c2.hash_val || c1.hash_val2 != c2.hash_val2) return false;
            }
            return true; //have enough confidence that 2 hash values are enough to ensure its uniqueness
            /*
            if (!JUST_CHECK_OCCUPIED) return c1.cells == c2.cells;
            for (int i = 0; i < c1.canvas_rows; ++i) {
                for (int j = 0; j < c1.canvas_cols; ++j) {
                    if (c1.cells[i][j].GetBlankBlockType() != c2.cells[i][j].GetBlankBlockType()) return false;
                }
            }
            return true;
            */
        }
    };

    bool operator==(const Canvas& c2) const {
        return CanvasEqual<false>()(*this, c2);
    }

    void Clean() {
        canvas_stale = true;
        for (auto& row : cells) {
            for (auto& c : row) {
                c.Clean();
            }
        }
        used_colors.clear();
    }

    void Rotate(RotateType rt, bool just_mark = false) {
        canvas_stale = true;
        if (RT_NONE == rt) return;
        auto rotate_clockwise_90_fn = [&]() {
            int rows = GetCanvasHeight();
            int cols = GetCanvasWidth();
            vector<vector<Cell>> cells2(cols, vector<Cell>(rows));
            for (int i = 0; i < rows; ++i) {
                for (int j = 0; j < cols; ++j) {
                    cells2[j][rows - 1 - i] = cells[i][j];
                    cells2[j][rows - 1 - i].Rotate(RT_CW_90, !just_mark);
                }
            }
            cells = cells2;
            canvas_rows = cols;
            canvas_cols = rows;
        };
        auto rotate_h_flip_fn = [&]() {
            int rows = GetCanvasHeight();
            int cols = GetCanvasWidth();
            for (int i = 0; i < rows; ++i) {
                for (int j = 0; j < cols / 2; ++j) {
                    swap(cells[i][j], cells[i][cols - 1 - j]);
                    cells[i][j].Rotate(RT_H_FLIP, !just_mark);
                    cells[i][cols - 1 - j].Rotate(RT_H_FLIP, !just_mark);
                }
                if (cols % 2) cells[i][cols / 2].Rotate(RT_H_FLIP, !just_mark);
            }
        };

        switch (rt) {
        case RT_CW_270:
            rotate_clockwise_90_fn();
        case RT_CW_180:
            rotate_clockwise_90_fn();
        case RT_CW_90:
            rotate_clockwise_90_fn();
            break;
        case RT_H_FLIP_CW_90:
            rotate_clockwise_90_fn();
        case RT_H_FLIP_CW_180:
            rotate_clockwise_90_fn();
        case RT_H_FLIP_CW_270:
            rotate_clockwise_90_fn();
        case RT_H_FLIP:
            rotate_h_flip_fn();
            break;
        default: break;
        }
    }

    inline WallsInfo AnalyseWallsAroundCell(int r, int c) const {
        WallsInfo res;

        auto blank_bt = cells[r][c].GetBlankBlockType();
        res.Set(DT_NONE, BT_NONE == blank_bt); //current block is blocked if there's no left space

        if (0 == r) res.Set(DT_UP, true);
        else {
            auto blank_bt = cells[r - 1][c].GetBlankBlockType();
            res.Set(DT_UP, BT_NONE == blank_bt || BT_RIGHT_UP == blank_bt || BT_LEFT_UP == blank_bt);
        }

        if (0 == c) res.Set(DT_LEFT, true);
        else {
            auto blank_bt = cells[r][c - 1].GetBlankBlockType();
            res.Set(DT_LEFT, BT_NONE == blank_bt || BT_LEFT_DOWN == blank_bt || BT_LEFT_UP == blank_bt);
        }

        if (canvas_rows - 1 == r) res.Set(DT_DOWN, true);
        else {
            auto blank_bt = cells[r + 1][c].GetBlankBlockType();
            res.Set(DT_DOWN, BT_NONE == blank_bt || BT_LEFT_DOWN == blank_bt || BT_RIGHT_DOWN == blank_bt);
        }

        if (canvas_cols - 1 == c) res.Set(DT_RIGHT, true);
        else {
            auto blank_bt = cells[r][c + 1].GetBlankBlockType();
            res.Set(DT_RIGHT, BT_NONE == blank_bt || BT_RIGHT_UP == blank_bt || BT_RIGHT_DOWN == blank_bt);
        }

        return res;
    }

    vector<Canvas> GetCanvasesAfterRotate(bool just_mark = false, bool keep_shape = false) const;

    int canvas_rows = 0;
    int canvas_cols = 0;
    vector<vector<Cell>> cells;
    set<int> used_colors;

    bool canvas_stale = false;
    size_t hash_val = 0;
    size_t hash_val2 = 0;
    size_t hash_val_just_check_occupied = 0;
    size_t hash_val2_just_check_occupied = 0;
};

void PrintCanvases(const vector<Canvas>& canvases, bool padding) {
    if (canvases.empty()) return;
    int canvas_rows = canvases[0].GetCanvasHeight();
    for (auto& c : canvases) assert(c.GetCanvasHeight() == canvas_rows);
    Canvas::Cell pad_cell;
    for (int i = 0; i < canvas_rows; ++i) {
        for (int j = 0; j < (int)canvases.size(); ++j) {
            if (j > 0 && padding) printf("%s", pad_cell.cell_lines[0].data());
            for (auto& cell : canvases[j].GetRow(i)) { printf("%s", cell.cell_lines[0].data()); }
        }
        printf("\n");

        for (int j = 0; j < (int)canvases.size(); ++j) {
            if (j > 0 && padding) printf("%s", pad_cell.cell_lines[1].data());
            for (auto& cell : canvases[j].GetRow(i)) { printf("%s", cell.cell_lines[1].data()); }
        }
        printf("\n");

        for (int j = 0; j < (int)canvases.size(); ++j) {
            if (j > 0 && padding) printf("%s", pad_cell.cell_lines[2].data());
            for (auto& cell : canvases[j].GetRow(i)) { printf("%s", cell.cell_lines[2].data()); }
        }
        printf("\n");
    }
    printf("\n");
}

void Canvas::DedupSnapshots(const vector<Model>& models, vector<Canvas::Snapshot>& snapshots) const {
    vector<Snapshot> final_snapshots;
    Canvas c2(canvas_rows, canvas_cols);
    unordered_set<Canvas, Canvas::CanvasHash<>> all_solutions; //considering rotate
    for (auto& ss : snapshots) {
        c2.Clean();
        assert(c2.Repaint(models, ss, true));
        int new_solution = 0;
        for (auto& c : c2.GetCanvasesAfterRotate(true, true)) {
            if (all_solutions.insert(c).second && 0 == new_solution++) {
                final_snapshots.push_back(ss);
            }
        }
    }
    snapshots.swap(final_snapshots);
}

void Shape::Print() const {
    auto [rows, cols] = GetCircumRectangleSize();
    Canvas canvas(rows, cols);
    canvas.AddShape(*this);
    canvas.Print();
}

bool Shape::operator==(const Shape& s2) const {
    if (hash_val != s2.hash_val || hash_val2 != s2.hash_val2) return false;
    return true; //have enough confidence that 2 hash values are enough to ensure its uniqueness
    /*
    if (blks.size() != s2.blks.size()) return false;
    if (rows != s2.rows) return false;
    if (cols != s2.cols) return false;

    Canvas c1(rows, cols);
    Canvas c2(rows, cols);

    assert(c1.AddShape(*this, -1, -1, CT_NONE, true));
    assert(c2.AddShape(s2, -1, -1, CT_NONE, true));
    return c1 == c2;
    */
}

WallsInfo Shape::AnalyseWallsAroundBlock(int blk_idx) const {
    Canvas canvas(rows + 2, cols + 2);
    assert(canvas.AddShape(*this, blk0_offset_r + 1, blk0_offset_c + 1, CT_NONE, true));

    auto& b = blks[blk_idx];
    int r = blk0_offset_r + b.rel_r + 1;
    int c = blk0_offset_c + b.rel_c + 1;
    return canvas.AnalyseWallsAroundCell(r, c);
}

void Model::Print() const {
    const auto& shapes = GetShapes();
    vector<pair<int, int>> outline_sizes;
    int max_height = 0;
    int total_width = shapes.size() - 1;
    for (auto& s : shapes) outline_sizes.push_back(s.GetCircumRectangleSize());
    for (auto& x : outline_sizes) {
        max_height = max(max_height, x.first);
        total_width += x.second;
    }

    const int one_cell_width = (strlen(Canvas::Cell::top_line) + 2 * strlen(Canvas::Cell::top_corner));
    char buf[total_width * one_cell_width];
    memset(buf, ' ', total_width * one_cell_width);
    for (int i = 0, c = 0; i < (int)shapes.size(); ++i) {
        const auto& offset = shapes[i].GetStartBlockOffset();
        auto len = sprintf(&buf[c * one_cell_width], "[%d](%d,%d)", i, offset.first, offset.second);
        if (i + 1 != (int)shapes.size()) buf[c * one_cell_width + len] = ' ';
        c += outline_sizes[i].second + 1;
    }
    printf("%s\n", ColorText(buf, GetModelColor()).data());

    Canvas canvas(max_height, total_width);
    for (int i = 0, c = 0; i < (int)shapes.size(); ++i) {
        const auto& offset = shapes[i].GetStartBlockOffset();
        canvas.AddShape(shapes[i], 0 + offset.first, c + offset.second, GetModelColor(), false, true);
        c += outline_sizes[i].second + 1;
    }

    canvas.Print();
}

vector<Canvas> Canvas::GetCanvasesAfterRotate(bool just_mark, bool keep_shape) const {
    vector<Canvas> res;
    unordered_set<Canvas, Canvas::CanvasHash<>> us;
    if (keep_shape) {
        if (canvas_rows == canvas_cols) keep_shape = false;
    }
    for (int rt = RT_NONE; rt < RT_MAX; ++rt) {
        if (keep_shape && (RT_CW_90 == rt || RT_CW_270 == rt || RT_H_FLIP_CW_90 == rt || RT_H_FLIP_CW_270 == rt)) continue;
        Canvas c2 = *this;
        c2.Rotate((RotateType)rt, just_mark);
        if (us.insert(c2).second) res.push_back(move(c2));
    }
    return res;
}

int main(int argc, char* argv[]) {
    vector<string> model_strs = {
        string_printf("%s;%d", "s^s>s>ld",      CT_BRIGHT_GREEN),   //0, light green
        string_printf("%s;%d", "ru>s>svlu<ru",  CT_CYAN),           //1, cyan
        string_printf("%s;%d", "svsvlu<ru^rd",  CT_BRIGHT_MAGENTA), //2, pink
        string_printf("%s;%d", "svs>ld",        CT_RED),            //3, red
        string_printf("%s;%d", "lu^s>s>ld",     CT_BRIGHT_BLUE),    //4, dark blue
        string_printf("%s;%d", "ld<rdvru>s>ld", CT_MAGENTA),        //5, purple
        string_printf("%s;%d", "ru>s>ldvs<ru",  CT_BRIGHT_RED),     //6, orange
        string_printf("%s;%d", "s>s>ld",        CT_GREEN),          //7, dark green
        string_printf("%s;%d", "ru>s>ld",       CT_BRIGHT_CYAN),    //8, light blue
        string_printf("%s;%d", "ru>s^s^rd",     CT_BRIGHT_YELLOW)   //9, yellow
    };

    vector<Model> models;
    for (auto& ms : model_strs) models.emplace_back(ms);
    for (int i = 0; i < (int)models.size(); ++i) {
        printf("%s\n", ColorText(string_printf("\
================================================================================ \
model %d \
================================================================================", i), models[i].GetModelColor()).data());
        models[i].Print();
    }

    const bool is_heart_shape = false;
    Canvas canvas(5 + is_heart_shape, 6);
    if (is_heart_shape) {
        canvas.GetCell(0, 0).PaintBlock(BT_LEFT_UP, CT_NONE, true);
        canvas.GetCell(5, 5).PaintBlock(BT_RIGHT_DOWN, CT_NONE, true);

        canvas.GetCell(0, 3).PaintBlock(BT_RIGHT_UP, CT_NONE, true);
        canvas.GetCell(0, 4).PaintBlock(BT_SQUARE, CT_NONE, true);
        canvas.GetCell(0, 5).PaintBlock(BT_SQUARE, CT_NONE, true);
        canvas.GetCell(1, 4).PaintBlock(BT_SQUARE, CT_NONE, true);
        canvas.GetCell(1, 5).PaintBlock(BT_SQUARE, CT_NONE, true);
        canvas.GetCell(2, 5).PaintBlock(BT_RIGHT_UP, CT_NONE, true);
    }

    {
        set<int> used_model_idxs;
        auto fn = [&](int model_idx, int shape_idx, int r, int c) {
            auto& m = models[model_idx];
            assert(canvas.AddShape(m.GetShapes()[shape_idx], r, c, m.GetModelColor()));
            used_model_idxs.insert(model_idx);
        };
        (void)fn;

        //fn(1, 0, 0, 2);

        if (!used_model_idxs.empty()) {
            printf("the init graph is:\n");
            canvas.Print();

            vector<Model> tmp_models;
            for (int i = 0; i < (int)models.size(); ++i) {
                if (used_model_idxs.end() != used_model_idxs.find(i)) continue;
                tmp_models.push_back(move(models[i]));
            }
            tmp_models.swap(models);
        }
    }

    vector<Canvas::Snapshot> snapshots;
    if (!canvas.GetFillCanvasSnapshots_Opt(models, snapshots, -1, false)) {
    //if (!canvas.GetFillCanvasSnapshots(models, snapshots, -1, false)) {
        printf("Can't find a solution\n");
        return -1;
    }

    //for (auto& snapshot : snapshots) printf("%s\n", Canvas::GetDebugString(snapshot).data());

    printf("total %lu solutions, and just print one solution here:\n", snapshots.size());
    if (snapshots.size() > 10) {
        //const auto& hardest_snapshots = Canvas::AnalyseDifficultSnapshots(snapshots);
        //canvas.Repaint(models, hardest_snapshots[0]);
        canvas.Repaint(models, snapshots[0]);
        canvas.Print(true);
    } else {
        for (auto& snapshot : snapshots) {
            Canvas c2 = canvas;
            c2.Repaint(models, snapshot);
            c2.Print(true);
        }
    }

    return 0;
}
