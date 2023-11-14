package tokens

// ---------------------------------------------------------------------------------------------------------------------

type TokenHolder struct {
	tokens []Token
	idx    int
}

func NewTokenHolder() *TokenHolder {
	return &TokenHolder{tokens: make([]Token, 0), idx: 0}
}

// ---------------------------------------------------------------------------------------------------------------------

func (th *TokenHolder) AppendTokensFrom(oth *TokenHolder) {
	th.tokens = append(th.tokens, oth.tokens...)
}
func (th *TokenHolder) AppendToken(token Token) {
	th.tokens = append(th.tokens, token)
}

// ---------------------------------------------------------------------------------------------------------------------

func (th *TokenHolder) GetNextToken() (token *Token) {
	token = &(th.tokens[th.idx])
	th.idx++
	return
}

func (th *TokenHolder) NextToken() *Token {
	return &(th.tokens[th.idx])
}

// ---------------------------------------------------------------------------------------------------------------------

func (th *TokenHolder) Empty() bool {
	return th.idx >= len(th.tokens)
}

func (th *TokenHolder) Reset() {
	th.idx = 0
}
