from openpyxl import load_workbook
import cairo
from random import shuffle
from textwrap import TextWrapper

CATEGORIES = [
    "geography",
    "entertainment",
    "history",
    "art",
    "science",
    "sport"
]
TAU = 6.2831
WIDTH = 720
HEIGHT = 500

norm = lambda col: [v / 255 for v in col]  # noqa: E731
BLUE = norm([53, 79, 157])
PINK = norm([234, 66, 161])
YELLOW = norm([234, 226, 66])
BROWN = norm([64, 34, 34])
GREEN = norm([21, 69, 5])
ORANGE = norm([210, 93, 3])


def read_qas(filename="Trivia.xlsx"):
    wb = load_workbook(filename)
    ws = wb.active

    rows = list(ws.iter_rows(min_row=2, max_col=6, values_only=True))
    questions = [
        list(map(str, question)) for idx, question in enumerate(rows) if idx % 2 == 0  # noqa: E501
    ]
    answers = [
        list(map(str, answer)) for idx, answer in enumerate(rows) if idx % 2 == 1  # noqa: E501
    ]

    final = {cat: list() for cat in CATEGORIES}
    mapping = {i: v for i, v in enumerate(CATEGORIES)}

    for que, ans in zip(questions, answers):
        for i, _ in enumerate(CATEGORIES):
            card = {}
            card["question"] = que[i]
            card["answer"] = ans[i]
            category = mapping[i]
            final[category].append(card)

    return final


def draw_card(card, type, card_name):
    """A card is a set of 5 pieces of text
    It can be an answer card or a question card"""
    surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, WIDTH, HEIGHT)
    ctx = cairo.Context(surface)

    # Setting bg colour
    ctx.set_source_rgb(1, 1, 1)  # White
    ctx.rectangle(0, 0, WIDTH, HEIGHT)
    ctx.fill()

    # font settings
    ctx.select_font_face(
        "Open Sans",
        cairo.FONT_SLANT_NORMAL,
        cairo.FONT_WEIGHT_NORMAL
    )
    ctx.set_font_size(15)

    def ellipse(x, y, col):
        ctx.arc(x, y, HEIGHT * 0.05, 0, 6.28)
        ctx.set_source_rgb(*col)
        ctx.fill()

    X_MARGIN = WIDTH * 0.075

    cols = [BLUE, PINK, YELLOW, BROWN, GREEN, ORANGE]
    ctx.translate(0, HEIGHT * 0.1)
    height_without_margin = HEIGHT - (HEIGHT * 0.2)

    wrapper = TextWrapper()
    wrapper.width = 90

    for idx, text in enumerate(card):
        ellipse(X_MARGIN, height_without_margin / 5 * idx, cols[idx])
        ctx.move_to(X_MARGIN + WIDTH * 0.05, height_without_margin / 5 * idx)
        ctx.set_source_rgb(0, 0, 0)  # Black

        wrapped_text = wrapper.wrap(text)
        for wrap_idx, line in enumerate(wrapped_text):
            ctx.move_to(
                X_MARGIN + WIDTH * 0.05,
                height_without_margin / 5 * idx + wrap_idx * 15)
            ctx.show_text(line)

    surface.write_to_png(card_name)


def gen_qas():
    questions = read_qas()
    for key in questions:
        shuffle(questions[key])

    # While there are still questions
    while questions["geography"]:
        q_card = []
        a_card = []
        for cat in CATEGORIES:
            qa = questions[cat].pop()
            q_card.append(qa["question"])
            a_card.append(qa["answer"])

        yield q_card, a_card


for idx, (q_card, a_card) in enumerate(gen_qas()):
    draw_card(q_card, "NULL", f"{idx}_question.png")
    draw_card(a_card, "NULL", f"{idx}_answer.png")
