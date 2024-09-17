package com.cosmetic.gg.service.product;

import java.util.List;

import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.dto.response.product.ProductItemResponse;
import com.cosmetic.gg.entity.product.Cart;

public interface CartService {
	
	List<ProductItemResponse> search(String id);

	List<Error> validator(Cart cart);
	
	Cart addProduct(Cart cartModel);
	
	Cart updateProduct(String id, Integer quantity);
	
	Error removeProduct(String id);
	
	Error deleteMulti(List<String> ids);
}
