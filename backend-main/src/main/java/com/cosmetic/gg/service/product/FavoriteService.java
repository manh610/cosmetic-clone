package com.cosmetic.gg.service.product;

import java.util.List;

import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.dto.response.product.ProductResponse;
import com.cosmetic.gg.entity.product.Favorite;

public interface FavoriteService {
	
	List<ProductResponse> search(String id);

	List<Error> validator(Favorite favorite);
	
	Favorite addProduct(Favorite favorite);
	
	Error removeProduct (String userId, String productId);
}
