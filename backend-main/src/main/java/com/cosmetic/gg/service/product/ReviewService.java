package com.cosmetic.gg.service.product;

import java.util.List;

import org.springframework.web.multipart.MultipartFile;

import com.cosmetic.gg.common.enums.ErrorCode;
import com.cosmetic.gg.common.model.Error;
import com.cosmetic.gg.dto.response.product.ReviewResponse;
import com.cosmetic.gg.entity.product.Review;

public interface ReviewService {
	
	List<ReviewResponse> search(String id, Integer star);
	
	List<Error> validator(Review review);
	
	Review create(Review review);
	
	Error delete(String id);
	
	ErrorCode changeImage(List<MultipartFile> images, String id);

}
