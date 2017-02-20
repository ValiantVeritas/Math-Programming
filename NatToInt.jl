function nattoint(n)
	if (n % 2) == 0 
		return (-1 * (n/2))
    else
    	return ((n+1)/2)
    end
 end

function flip(x)
	return (-1)^float(x)
end

function ntoz(x)
	oddoreven = (flip(x-1) + 1)/2 # returns 1 for odd numbers and 0 for even numbers
	return flip(x) * (x+oddoreven(x))/2
end
