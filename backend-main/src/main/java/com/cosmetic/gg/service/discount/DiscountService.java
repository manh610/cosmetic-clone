package com.cosmetic.gg.service.discount;

import java.util.List;
import java.util.Map;

import org.springframework.web.multipart.MultipartFile;

import com.cosmetic.gg.common.enums.EDiscountType;
import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.dto.request.discount.ProductDiscountRequest;
import com.cosmetic.gg.dto.request.discount.ProductItemDiscountRequest;
import com.cosmetic.gg.dto.request.discount.UserDiscountRequest;
import com.cosmetic.gg.dto.response.discount.DiscountResponse;
import com.cosmetic.gg.entity.discount.Discount;
import com.cosmetic.gg.entity.discount.ProductItemDiscount;
import com.cosmetic.gg.entity.discount.UserDiscount;

public interface DiscountService {
	
	Map<String, Object> search(String keyword, EDiscountType discountType, 
			Integer pageIndex, Integer pageSize);

	List<Error> validator(Discount discountModel);
	
	Discount create(Discount discountModel);
	
	Discount update(Discount discountModel);
	
	Error delete(String id);
	
	ErrorCode changeImage(MultipartFile image, String id);
	
	Error addUserDiscount(UserDiscountRequest discountRequest);
	
//	Error deleteAllUserDiscount(String id);
	
	Error addProductDiscount(ProductDiscountRequest productDiscountRequest);
	
	Error addProductItemDiscount(ProductItemDiscountRequest productDiscountRequest);
	
//	Error deleteAllProductDiscount(String id);
//	
	DiscountResponse detail(String id);
}
